{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE LambdaCase          #-}
module Haskell.Ide.Engine.Plugin.HieExtras
  ( getDynFlags
  , getCompletions
  , getTypeForName
  , getSymbolsAtPoint
  , getReferencesInDoc
  , getModule
  , findDef
  , showName
  , safeTyThingId
  ) where

import           ConLike
import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Control.Lens
import           Data.Aeson
import           Data.IORef
import qualified Data.List                                    as List
import qualified Data.Map                                     as Map
import           Data.Maybe
#if __GLASGOW_HASKELL__ < 804
import           Data.Monoid
#endif
import qualified Data.Text                                    as T
import           Data.Typeable
import           DataCon
import           Exception
import           FastString
import           Finder
import           GHC
import qualified GhcMod.Error                                 as GM
import qualified GhcMod.LightGhc                              as GM
import           Haskell.Ide.Engine.ArtifactMap
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginUtils
import qualified Haskell.Ide.Engine.Plugin.Fuzzy              as Fuzzy
import           HscTypes
import qualified Language.Haskell.LSP.Types                   as J
import           Language.Haskell.Refact.API                 (showGhc)
import           Language.Haskell.Refact.Utils.MonadFunctions
import           Name
import           Outputable                                   (Outputable)
import qualified Outputable                                   as GHC
import qualified DynFlags                                     as GHC
import           Packages
import           SrcLoc
import           TcEnv
import           Var

getDynFlags :: Uri -> IDErring IdeM DynFlags
getDynFlags uri = do
  fp <- pluginGetFile "getDynFlags: " uri
  ms_hspp_opts . pm_mod_summary . tm_parsed_module . tcMod <$> fetchCachedModule fp

-- ---------------------------------------------------------------------

data NameMapData = NMD
  { inverseNameMap ::  !(Map.Map Name [SrcSpan])
  } deriving (Typeable)

invert :: (Ord v) => Map.Map k v -> Map.Map v [k]
invert m = Map.fromListWith (++) [(v,[k]) | (k,v) <- Map.toList m]

instance ModuleCache NameMapData where
  cacheDataProducer cm = pure $ NMD inm
    where nm  = initRdrNameMap $ tcMod cm
          inm = invert nm

-- ---------------------------------------------------------------------

data CompItem = CI
  { origName     :: Name
  , importedFrom :: T.Text
  , thingType    :: Maybe T.Text
  , label        :: T.Text
  } deriving (Show)

instance Eq CompItem where
  (CI n1 _ _ _) == (CI n2 _ _ _) = n1 == n2

instance Ord CompItem where
  compare (CI n1 _ _ _) (CI n2 _ _ _) = compare n1 n2

occNameToComKind :: OccName -> J.CompletionItemKind
occNameToComKind oc
  | isVarOcc  oc = J.CiFunction
  | isTcOcc   oc = J.CiClass
  | isDataOcc oc = J.CiConstructor
  | otherwise    = J.CiVariable

type HoogleQuery = T.Text

mkQuery :: T.Text -> T.Text -> HoogleQuery
mkQuery name importedFrom = name <> " module:" <> importedFrom
                                 <> " is:exact"

mkCompl :: CompItem -> J.CompletionItem
mkCompl CI{origName,importedFrom,thingType,label} =
  J.CompletionItem label kind (Just $ maybe "" (<>"\n") thingType <> importedFrom)
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing hoogleQuery
  where kind  = Just $ occNameToComKind $ occName origName
        hoogleQuery = Just $ toJSON $ mkQuery label importedFrom

mkModCompl :: T.Text -> J.CompletionItem
mkModCompl label =
  J.CompletionItem label (Just J.CiModule) Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing hoogleQuery
  where hoogleQuery = Just $ toJSON $ "module:" <> label

safeTyThingId :: TyThing -> Maybe Id
safeTyThingId (AnId i)                    = Just i
safeTyThingId (AConLike (RealDataCon dc)) = Just $ dataConWrapId dc
safeTyThingId _                           = Nothing

-- Associates a module's qualifier with its members
type QualCompls = Map.Map T.Text [CompItem]

data CachedCompletions = CC
  { allModNamesAsNS :: [T.Text]
  , unqualCompls :: [CompItem]
  , qualCompls :: QualCompls
  } deriving (Typeable)

instance ModuleCache CachedCompletions where
  cacheDataProducer cm = do
    let tm = tcMod cm
        parsedMod = tm_parsed_module tm
        curMod = moduleName $ ms_mod $ pm_mod_summary parsedMod
        Just (_,limports,_,_) = tm_renamed_source tm

        iDeclToModName :: ImportDecl name -> ModuleName
        iDeclToModName = unLoc . ideclName

        showModName :: ModuleName -> T.Text
        showModName = T.pack . moduleNameString

#if __GLASGOW_HASKELL__ >= 802
        asNamespace :: ImportDecl name -> ModuleName
        asNamespace imp = fromMaybe (iDeclToModName imp) (fmap GHC.unLoc $ ideclAs imp)
#else
        asNamespace :: ImportDecl name -> ModuleName
        asNamespace imp = fromMaybe (iDeclToModName imp) (ideclAs imp)
#endif
        -- Full canonical names of imported modules
        importDeclerations = map unLoc limports

        -- The given namespaces for the imported modules (ie. full name, or alias if used)
        allModNamesAsNS = map (showModName . asNamespace) importDeclerations

        typeEnv = md_types $ snd $ tm_internals_ tm
        toplevelVars = mapMaybe safeTyThingId $ typeEnvElts typeEnv
        varToCompl var = CI name (showModName curMod) typ label
          where
            typ = Just $ T.pack $ showGhc $ varType var
            name = Var.varName var
            label = T.pack $ showGhc name

        toplevelCompls = map varToCompl toplevelVars

        toCompItem :: ModuleName -> Name -> CompItem
        toCompItem mn n =
          CI n (showModName mn) Nothing (T.pack $ showGhc n)

        allImportsInfo :: [(Bool, T.Text, ModuleName, Maybe (Bool, [Name]))]
        allImportsInfo = map getImpInfo importDeclerations
          where
            getImpInfo imp =
              let modName = iDeclToModName imp
                  modQual = showModName (asNamespace imp)
                  isQual = ideclQualified imp
                  hasHiddsMembers =
                    case ideclHiding imp of
                      Nothing -> Nothing
                      Just (hasHiddens, L _ liens) ->
                        Just (hasHiddens, concatMap (ieNames . unLoc) liens)
              in (isQual, modQual, modName, hasHiddsMembers)

        getModCompls :: GhcMonad m => HscEnv -> m ([CompItem], QualCompls)
        getModCompls hscEnv = do
          (unquals, qualKVs) <- foldM (orgUnqualQual hscEnv) ([], []) allImportsInfo
          return (unquals, Map.fromList qualKVs)

        orgUnqualQual hscEnv (prevUnquals, prevQualKVs) (isQual, modQual, modName, hasHiddsMembers) =
          let
            ifUnqual xs = if isQual then prevUnquals else (prevUnquals ++ xs)
            setTypes = setComplsType hscEnv
          in
            case hasHiddsMembers of
              Just (False, members) -> do
                compls <- setTypes (map (toCompItem modName) members)
                return
                  ( ifUnqual compls
                  , (modQual, compls) : prevQualKVs
                  )
              Just (True , members) -> do
                let hiddens = map (toCompItem modName) members
                allCompls <- getComplsFromModName modName
                compls <- setTypes (allCompls List.\\ hiddens)
                return
                  ( ifUnqual compls
                  , (modQual, compls) : prevQualKVs
                  )
              Nothing -> do
                -- debugm $ "///////// Nothing " ++ (show modQual)
                compls <- setTypes =<< getComplsFromModName modName
                return
                  ( ifUnqual compls
                  , (modQual, compls) : prevQualKVs
                  )

        getComplsFromModName :: GhcMonad m
          => ModuleName -> m [CompItem]
        getComplsFromModName mn = do
          mminf <- getModuleInfo =<< findModule mn Nothing
          return $ case mminf of
            Nothing -> []
            Just minf -> map (toCompItem mn) $ modInfoExports minf

        setComplsType :: (Traversable t, MonadIO m)
          => HscEnv -> t CompItem -> m (t CompItem)
        setComplsType hscEnv xs =
          liftIO $ forM xs $ \ci@CI{origName} -> do
            mt <- (Just <$> lookupGlobal hscEnv origName)
                    `catch` \(_ :: SourceError) -> return Nothing
            let typ = do
                  t <- mt
                  tyid <- safeTyThingId t
                  return $ T.pack $ showGhc $ varType tyid
            return $ ci {thingType = typ}

    hscEnvRef <- use ghcSession
    hscEnv <- liftIO $ traverse readIORef hscEnvRef
    (unquals, quals) <- maybe
                          (pure ([], Map.empty))
                          (\env -> GM.runLightGhc env (getModCompls env))
                          hscEnv
    return $ CC
      { allModNamesAsNS = allModNamesAsNS
      , unqualCompls = toplevelCompls ++ unquals
      , qualCompls = quals
      }

getCompletions :: Uri -> (T.Text, T.Text) -> IDErring IdeM [J.CompletionItem]
getCompletions uri (qualifier, ident) = pluginGetFile "getCompletions: " uri >>= \file ->
  let handlers =
        [ GM.GHandler $ \(ex :: SomeException) ->
            ideError PluginError (T.pack $ "getCompletions" <> ": " <> (show ex)) Null
        ]
  in flip GM.gcatches handlers $ do
    -- debugm $ "got prefix" ++ show (qualifier, ident)
    let enteredQual = if T.null qualifier then "" else qualifier <> "."
        fullPrefix = enteredQual <> ident
    (_, CC { allModNamesAsNS, unqualCompls, qualCompls }) <- fetchCachedModuleAndData file
    let filtModNameCompls = map mkModCompl
          $ mapMaybe (T.stripPrefix enteredQual)
          $ Fuzzy.simpleFilter fullPrefix allModNamesAsNS

        filtCompls = Fuzzy.filterBy label ident $ if T.null qualifier
          then unqualCompls
          else Map.findWithDefault [] qualifier qualCompls

    return $ filtModNameCompls ++ map mkCompl filtCompls

-- ---------------------------------------------------------------------

getTypeForName :: Name -> IdeM (Maybe Type)
getTypeForName n = do
  hscEnvRef <- use ghcSession
  mhscEnv <- liftIO $ traverse readIORef hscEnvRef
  case mhscEnv of
    Nothing -> return Nothing
    Just hscEnv -> do
      mt <- liftIO $ (Just <$> lookupGlobal hscEnv n)
                        `catch` \(_ :: SomeException) -> return Nothing
      return $ fmap varType $ safeTyThingId =<< mt

-- ---------------------------------------------------------------------

getSymbolsAtPoint :: Uri -> Position -> IDErring IdeM [(Range, Name)]
getSymbolsAtPoint uri pos = do
  file <- pluginGetFile "getSymbolsAtPoint: " uri
  getSymbolsAtPointPure pos <$> fetchCachedModule file

getSymbolsAtPointPure :: Position -> CachedModule -> [(Range,Name)]
getSymbolsAtPointPure pos cm = maybe [] (`getArtifactsAtPos` locMap cm) $ newPosToOld cm pos

symbolFromTypecheckedModule
  :: LocMap
  -> Position
  -> Maybe (Range, Name)
symbolFromTypecheckedModule lm pos =
  listToMaybe $ getArtifactsAtPos pos lm

-- ---------------------------------------------------------------------

-- | Find the references in the given doc, provided it has been
-- loaded.  If not, return the empty list.
getReferencesInDoc :: Uri -> Position -> IDErring IdeM [J.DocumentHighlight]
getReferencesInDoc uri pos = do
  file <- pluginGetFile "getReferencesInDoc: " uri
  either (const []) refs <$> fetchCachedModuleAndDataEither file
  where
    refs (cm, NMD{inverseNameMap}) =
      [ J.DocumentHighlight r' (Just kind)
      | let lm = locMap cm
            pm = tm_parsed_module $ tcMod cm
            cfile = ml_hs_file $ ms_location $ pm_mod_summary pm
            mpos = newPosToOld cm pos
      , pos' <- maybeToList mpos
      , (_,name) <- getArtifactsAtPos pos' lm
      , let usages = fromMaybe [] $ Map.lookup name inverseNameMap
            defn = nameSrcSpan name
            defnInSameFile =
              (unpackFS <$> srcSpanFileName_maybe defn) == cfile
            mdefn
              | isVarOcc (occName name) && defnInSameFile = (defn :)
              | otherwise = id
      , spn <- mdefn usages
      , let kind = if spn == defn then J.HkWrite else J.HkRead
      , Right r <- [srcSpan2Range spn]
      , r' <- maybeToList $ oldRangeToNew cm r
      ]

-- ---------------------------------------------------------------------

showName :: Outputable a => a -> T.Text
showName = T.pack . prettyprint
  where
    prettyprint x = GHC.renderWithStyle GHC.unsafeGlobalDynFlags (GHC.ppr x) style
#if __GLASGOW_HASKELL__ >= 802
    style = (GHC.mkUserStyle GHC.unsafeGlobalDynFlags GHC.neverQualify GHC.AllTheWay)
#else
    style = (GHC.mkUserStyle GHC.neverQualify GHC.AllTheWay)
#endif

getModule :: DynFlags -> Name -> Maybe (Maybe T.Text,T.Text)
getModule df n = do
  m <- nameModule_maybe n
  let uid = moduleUnitId m
  let pkg = showName . packageName <$> lookupPackage df uid
  return (pkg, T.pack $ moduleNameString $ moduleName m)

-- ---------------------------------------------------------------------

-- | Return the definition
findDef :: Uri -> Position -> IDErring IdeM [Location]
findDef uri pos = do
  file <- pluginGetFile "findDef: " uri
  fmap maybeToList $ runMaybeT $ do
    Right cm <- lift $ fetchCachedModuleEither file
    let rfm = revMap cm
        lm = locMap cm
        mm = moduleMap cm
        oldPos = newPosToOld cm pos
    case (`getArtifactsAtPos` mm) <$> oldPos of
      Just ((_,mn):_) -> gotoModule rfm mn
      _ -> do
        Just (_, n) <- pure $ symbolFromTypecheckedModule lm =<< oldPos
        realSpan@(RealSrcSpan _) <- pure $ nameSrcSpan n
        lift (srcSpan2Loc rfm realSpan) >>= \case
          Right l@(J.Location luri range) ->
            (<|> pure l) $ do
              Just fp <- pure $ uriToFilePath luri
              ModuleCached cm' _ <- lift $ getCachedModule fp
              Just r <- pure $ oldRangeToNew cm' range
              pure $ J.Location luri r
          Left x -> do
            debugm "findDef: name srcspan not found/valid"
            ideError PluginError ("hare:findDef" <> ": \"" <> x <> "\"") Null
  where
    gotoModule :: (FilePath -> FilePath) -> ModuleName -> MaybeT (IDErring IdeM) Location
    gotoModule rfm mn = do
      
      hscEnvRef <- use ghcSession
      mHscEnv <- liftIO $ traverse readIORef hscEnvRef

      case mHscEnv of
        Just env -> do
          Found (ModLocation (Just src) _ _) _ <- liftIO $ do
            -- Flush cache or else we get temporary files
            flushFinderCaches env
            findImportedModule env mn Nothing
          fp <- reverseMapFile rfm src

          let r = Range (Position 0 0) (Position 0 0)
          return $ Location (filePathToUri fp) r
        Nothing -> ideError PluginError "Couldn't get hscEnv when finding import" Null

