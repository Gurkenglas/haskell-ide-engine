{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Haskell.Ide.Engine.ModuleCache where
  
import           Control.Lens
import           Control.Concurrent.MVar.Lifted
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Control.Monad.State
import qualified Data.Aeson as J
import           Data.Dynamic (toDyn, fromDyn)
import           Data.Generics (Proxy(..), typeRep, typeOf)
import qualified Data.Map as Map
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           Exception (ExceptionMonad)
import           System.Directory
import           System.FilePath

import qualified GhcMod.Cradle as GM
import qualified GhcMod.Monad  as GM
import qualified GhcMod.Types  as GM

import           Haskell.Ide.Engine.PluginsIdeMonads
import           Haskell.Ide.Engine.GhcModuleCache

-- ---------------------------------------------------------------------
-- | Runs an IdeM action with the given Cradle
withCradle :: (GM.GmEnv m) => GM.Cradle -> m a -> m a
withCradle crdl =
  GM.gmeLocal (\env -> env {GM.gmCradle = crdl})

-- ---------------------------------------------------------------------
-- | Runs an action in a ghc-mod Cradle found from the
-- directory of the given file. If no file is found
-- then runs the action in the default cradle.
-- Sets the current directory to the cradle root dir
-- in either case
runActionWithContext :: (GM.GmEnv m, GM.MonadIO m, MonadIde m
                        , GM.GmLog m, MonadBaseControl IO m, ExceptionMonad m, GM.GmOut m)
                     => Maybe FilePath -> m a -> m a
runActionWithContext Nothing action = do
  crdl <- GM.cradle
  liftIO $ setCurrentDirectory $ GM.cradleRootDir crdl
  action
runActionWithContext (Just uri) action = do
  crdl <- getCradle uri
  liftIO $ setCurrentDirectory $ GM.cradleRootDir crdl
  withCradle crdl action

-- | Get the Cradle that should be used for a given URI
getCradle :: (GM.GmEnv m, GM.MonadIO m, MonadIde m, GM.GmLog m
             , MonadBaseControl IO m, ExceptionMonad m, GM.GmOut m)
          => FilePath -> m GM.Cradle
getCradle fp = do
      dir <- liftIO $ takeDirectory <$> canonicalizePath fp
      mcache <- use $ modules . cradleCache . 
      let mcradle = (Map.lookup dir . cradleCache) mcache
      case mcradle of
        Just crdl ->
          return crdl
        Nothing -> do
          opts <- GM.options
          crdl <- GM.findCradle' (GM.optPrograms opts) dir
          -- debugm $ "cradle cache miss for " ++ dir ++ ", generating cradle " ++ show crdl
          modifyCache (\s -> s { cradleCache = Map.insert dir crdl (cradleCache s)})
          return crdl


-- | The possible states the cache can be in
-- along with the cache or error if present
data CachedModuleResult = ModuleLoading
                        -- ^ The module has no cache yet and has not failed
                        | ModuleFailed T.Text
                        -- ^ The module has no cache because something went wrong
                        | ModuleCached CachedModule IsStale
                        -- ^ A cache exists for the module
type IsStale = Bool

-- | Returns true if there is a CachedModule for a given URI
isCached :: MonadIde m => FilePath -> m Bool
isCached = fmap (has $ _Just . _Right) . tryGetModule

-- Returns any present module cache for the given URI.
-- The data is associated with the CachedModule and its cache is
-- invalidated when a new CachedModule is loaded.
tryGetModule :: MonadIde m => FilePath -> m (Maybe UriCache)
tryGetModule = tryReadMVar <=< getModuleMVar

-- As above, but waits until the module is available.
getModule :: MonadIde m => FilePath -> m UriCache
getModule = readMVar <=< getModuleMVar
    {-
    Just UriCache{cachedModule = cm, cachedData = dat} ->
      case Map.lookup (typeRep $ (Proxy :: Proxy a)) dat of
        Nothing -> do
          val <- cacheDataProducer cm
          let dat' = Map.insert (typeOf val) (toDyn val) dat
          modifyCache $ uriCaches . at uri' ?~ UriCache cm dat' False
          return (cm, val)
        Just x -> return $ fromDyn x $ error "impossible"
    -}

getCacheData :: (MonadIO m, ModuleCache a) => FilePath -> m a
getCacheData uri = do
  mvar <- getModuleMVar
  modifyMVar mvar $ _Right . cachedData %%~ \case
    Nothing -> 

-- | Saves a module to the cache and executes any deferred
-- responses waiting on that module.
cacheModule :: FilePath -> CachedModule -> IdeGhcM ()
cacheModule uri cm = do
  uri' <- liftIO $ canonicalizePath uri
  provideModule uri' $ UriCache cm Map.empty False

-- | Marks a module that it failed to load and triggers
-- any deferred responses waiting on it
failModule :: FilePath -> T.Text -> IdeGhcM ()
failModule fp err = do
  fp' <- liftIO $ canonicalizePath fp
  nocache <- isNothing . Map.lookup fp' . uriCaches <$> getModuleCache
  when nocache $ -- If there's no cache for the module mark it as failed
    provideModule fp' $ UriCacheFailed err

provideModule :: FilePath -> _ -> IdeGhcM ()
provideModule uri cached = liftIde $ do
  -- execute any queued actions for the module
  mvar <- getModuleMVar uri
  putMVar mvar cached

-- | Saves a module to the cache without clearing the associated cache data - use only if you are
-- sure that the cached data associated with the module doesn't change
cacheModuleNoClear :: (GM.MonadIO m, MonadIde m)
            => FilePath -> CachedModule -> m ()
cacheModuleNoClear uri cm = do
  uri' <- liftIO $ canonicalizePath uri
  dat <- memoizeData cm
  modifyCache $ uriCaches %~ Map.insertWith
    (updateCachedModule cm)
    uri'
    (UriCache cm dat False)
  where
    updateCachedModule :: CachedModule -> UriCache -> UriCache -> UriCache
    updateCachedModule cm' _ = cachedModule .~ cm'

-- | Deletes a module from the cache
deleteCachedModule :: (GM.MonadIO m, MonadIde m) => FilePath -> m ()
deleteCachedModule uri = do
  uri' <- liftIO $ canonicalizePath uri
  modifyCache $ uriCaches . at uri' .~ Nothing

markCacheStale :: (GM.MonadIO m, MonadIde m) => FilePath -> m ()
markCacheStale uri = do
  uri' <- liftIO $ canonicalizePath uri
  modifyCache $ uriCaches . ix uri' . isStale .~ True

-- ---------------------------------------------------------------------
-- | A ModuleCache is valid for the lifetime of a CachedModule
-- It is generated on need and the cache is invalidated
-- when a new CachedModule is loaded.
-- Allows the caching of arbitary data linked to a particular
-- TypecheckedModule.
-- TODO: this name is confusing, given GhcModuleCache. Change it
class Typeable a => ModuleCache a where
    -- | Defines an initial value for the state extension
    cacheDataProducer :: (GM.MonadIO m, MonadState IdeState m)
                      => CachedModule -> m a

instance ModuleCache () where
    cacheDataProducer = const $ return ()
