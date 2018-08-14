{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DefaultSignatures #-}

-- | IdeGhcM and associated types
module Haskell.Ide.Engine.PluginsIdeMonads
  (
  -- * Plugins
    PluginId
  , CommandName
  , CommandFunc(..)
  , PluginDescriptor(..)
  , PluginCommand(..)
  , CodeActionProvider
  , DiagnosticProvider(..)
  , DiagnosticProviderFunc
  , DiagnosticTrigger(..)
  , HoverProvider
  , SymbolProvider
  , IdePlugins(..)
  -- * The IDE monad
  , IdeGhcM
  , IdeState(..)
  , IDErring(..)
  , runIDErring
  , MonadIde(..)
  , IdeM
  , IdeError(..)
  , IdeErrorCode(..)
  -- * LSP types
  , Uri(..)
  , uriToFilePath
  , filePathToUri
  , Position(..)
  , Range(..)
  , Location(..)
  , TextDocumentIdentifier(..)
  , TextDocumentPositionParams(..)
  , WorkspaceEdit(..)
  , Diagnostic(..)
  , DiagnosticSeverity(..)
  , PublishDiagnosticsParams(..)
  , List(..)
  , IDErrs(..)
  , defer
  , getModuleMVar
  , fromIDErring
  , modules, idePlugins, extensibleState, ghcSession
  ) where

import           System.Directory
import           Control.Concurrent.STM
import           Control.Concurrent.MVar.Lifted
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Control
import           Control.Monad.Morph
import           Control.Monad.Base
import           Control.Lens
import           Exception

import           Data.Aeson
import           Data.Dynamic (Dynamic)
import           Data.IORef
import qualified Data.Map as Map
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Typeable (TypeRep, Typeable)

import qualified GhcMod.Monad        as GM
import           GHC.Generics
import           GHC (HscEnv)

import           Haskell.Ide.Engine.MultiThreadState
import           Haskell.Ide.Engine.GhcModuleCache

import           Language.Haskell.LSP.Types.Capabilities
import           Language.Haskell.LSP.Types (CodeAction (..),
                                             CodeActionContext (..),
                                             Diagnostic (..),
                                             DiagnosticSeverity (..),
                                             DocumentSymbol (..),
                                             List (..),
                                             Hover (..),
                                             Location (..),
                                             Position (..),
                                             PublishDiagnosticsParams (..),
                                             Range (..),
                                             TextDocumentIdentifier (..),
                                             TextDocumentPositionParams (..),
                                             Uri (..),
                                             VersionedTextDocumentIdentifier(..),
                                             WorkspaceEdit (..),
                                             filePathToUri,
                                             uriToFilePath)


type PluginId = T.Text
type CommandName = T.Text

newtype CommandFunc a b = CmdSync (a -> IDErring IdeGhcM b)

data PluginCommand = forall a b. (FromJSON a, ToJSON b, Typeable b) =>
  PluginCommand { commandName :: CommandName
                , commandDesc :: T.Text
                , commandFunc :: CommandFunc a b
                }

type CodeActionProvider =  VersionedTextDocumentIdentifier
                        -> Maybe FilePath -- ^ Project root directory
                        -> Range
                        -> CodeActionContext
                        -> IDErring IdeM [CodeAction]

-- type DiagnosticProviderFunc = DiagnosticTrigger -> Uri -> IDErring IdeM (Map.Map Uri (S.Set Diagnostic)))
type DiagnosticProviderFunc
  = DiagnosticTrigger -> Uri -> IDErring IdeGhcM (Map.Map Uri (S.Set Diagnostic))

data DiagnosticProvider = DiagnosticProvider
     { dpTrigger :: S.Set DiagnosticTrigger -- AZ:should this be a NonEmptyList?
     , dpFunc    :: DiagnosticProviderFunc
     }

data DiagnosticTrigger = DiagnosticOnOpen
                       | DiagnosticOnChange
                       | DiagnosticOnSave
                       deriving (Show,Ord,Eq)

type HoverProvider = Uri -> Position -> IDErring IdeM [Hover]

type SymbolProvider = Uri -> IDErring IdeM [DocumentSymbol]

data PluginDescriptor =
  PluginDescriptor { pluginName               :: T.Text
                   , pluginDesc               :: T.Text
                   , pluginCommands           :: [PluginCommand]
                   , pluginCodeActionProvider :: Maybe CodeActionProvider
                   , pluginDiagnosticProvider :: Maybe DiagnosticProvider
                   , pluginHoverProvider      :: Maybe HoverProvider
                   , pluginSymbolProvider     :: Maybe SymbolProvider
                   } deriving (Generic)

instance Show PluginCommand where
  show (PluginCommand name _ _) = "PluginCommand { name = " ++ T.unpack name ++ " }"

-- | a Description of the available commands stored in IdeGhcM
newtype IdePlugins = IdePlugins
  { ipMap :: Map.Map PluginId PluginDescriptor
  } deriving (Generic)

-- TODO:AZ this is a defective instance, do we actually need it?
-- Perhaps rather make a separate type explicitly for this purpose.
instance ToJSON IdePlugins where
  toJSON (IdePlugins m) = toJSON $ fmap (\x -> (commandName x, commandDesc x)) <$> fmap pluginCommands m

-- ---------------------------------------------------------------------

type IdeGhcM = GM.GhcModT IdeM

newtype IDErring m a = IDErring { getIDErring :: ExceptT IdeError m a }
  deriving (Functor, Applicative, Monad, MonadReader r, MonadState s
           , MonadIO, MonadTrans, MonadBase b, MFunctor)
instance GM.MonadIO m => GM.MonadIO (IDErring m) where
  liftIO = lift . GM.liftIO
instance GM.GmEnv m => GM.GmEnv (IDErring m) where
  gmeAsk = lift GM.gmeAsk
  gmeLocal f x = liftWith (\run -> GM.gmeLocal f $ run x) >>= restoreT . return
instance GM.GmLog m => GM.GmLog (IDErring m) where
  gmlJournal = lift . GM.gmlJournal
  gmlHistory = lift GM.gmlHistory
  gmlClear = lift GM.gmlClear
instance GM.GmOut m => GM.GmOut (IDErring m) where
  gmoAsk = lift GM.gmoAsk
instance GM.GmState m => GM.GmState (IDErring m) where
  gmsGet = lift GM.gmsGet
  gmsPut = lift . GM.gmsPut
  gmsState = lift . GM.gmsState

runIDErring :: IDErring m a -> m (Either IdeError a)
runIDErring = runExceptT . getIDErring

instance MonadTransControl IDErring where
  type StT IDErring a = StT (ExceptT IdeError) a
  liftWith = defaultLiftWith IDErring getIDErring
  restoreT = defaultRestoreT IDErring
instance MonadBaseControl b m => MonadBaseControl b (IDErring m) where
  type StM (IDErring m) a = ComposeSt IDErring m a
  liftBaseWith     = defaultLiftBaseWith
  restoreM         = defaultRestoreM

type IdeM = ReaderT ClientCapabilities (MultiThreadState IdeState)

class Monad m => MonadIde m where liftIde :: IdeM a -> m a
instance MonadIde IdeGhcM where liftIde = lift . lift
instance MonadIde m => MonadIde (IDErring m) where liftIde = lift . liftIde
instance MonadIde IdeM where liftIde = id

data IdeState = IdeState
  { _modules :: GhcModuleCache
  , _idePlugins  :: IdePlugins
  , _extensibleState :: !(Map.Map TypeRep Dynamic)
  , _ghcSession  :: Maybe (IORef HscEnv)
  }

-- | Error codes. Add as required
data IdeErrorCode
 = ParameterError          -- ^ Wrong parameter type
 | PluginError             -- ^ An error returned by a plugin
 | InternalError           -- ^ Code error (case not handled or deemed
                           --   impossible)
 | NoModuleAvailable       -- ^ No typechecked module available to use
 | UnknownPlugin           -- ^ Plugin is not registered
 | UnknownCommand          -- ^ Command is not registered
 | InvalidContext          -- ^ Context invalid for command
 | RequestCancelled        -- ^ A cancel request fired targeting this one
 | VersionMismatch         -- ^ The request expected another hie version
 | OtherError              -- ^ An error for which there's no better code
 deriving (Show,Read,Eq,Ord,Bounded,Enum,Generic)

instance ToJSON IdeErrorCode
instance FromJSON IdeErrorCode

-- | A more structured error than just a string
data IdeError = IdeError
 { ideCode    :: IdeErrorCode -- ^ The error code
 , ideMessage :: T.Text       -- ^ A human readable message
 , ideInfo    :: Value        -- ^ Additional information
 }
 deriving (Show,Read,Eq,Generic)

instance ToJSON IdeError
instance FromJSON IdeError

makeLenses ''IdeState

getModuleMVar :: FilePath -> IdeM (MVar UriCache)
getModuleMVar fp = do
  fp' <- liftIO $ canonicalizePath fp
  mmvar <- use $ modules . uriCaches . at fp'
  (\x -> maybe x pure mmvar) $ do
    mvar <- newEmptyMVar
    modules . uriCaches . at fp' <?= mvar

defer :: (MonadIde m, IDErrs m, MonadBase IO m) => FilePath -> m UriCache
defer = liftIde . getModuleMVar >=> readMVar

fromIDErring :: Monad m => (IdeError -> m a) -> IDErring m a -> m a
fromIDErring f = runIDErring >=> either f pure

class Monad m => IDErrs m where
  ideError :: IdeErrorCode -> T.Text -> Value -> m a
  default ideError :: (IDErrs n, MonadTrans t, m ~ t n) => IdeErrorCode -> T.Text -> Value -> m a
  ideError c m i = lift $ ideError c m i

instance Monad m => IDErrs (IDErring m) where
  ideError c m i = IDErring $ throwError $ IdeError c m i

instance IDErrs m => IDErrs (MaybeT m)

instance (MonadIO m, MonadBaseControl IO m) => ExceptionMonad (IDErring m) where
  gcatch act handler = control $ \run ->
      run act `gcatch` (run . handler)

  gmask = liftBaseOp gmask . liftRestore
    where liftRestore f r = f $ liftBaseOp_ r