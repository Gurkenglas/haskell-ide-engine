{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Haskell.Ide.Engine.GhcModuleCache where

import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Dynamic (Dynamic)
import           Data.Typeable (TypeRep)

import qualified GhcMod.Types                      as GM

import           GHC                               (TypecheckedModule)

import Haskell.Ide.Engine.ArtifactMap

import Language.Haskell.LSP.Types

type UriCaches = Map.Map FilePath UriCache

data UriCache = UriCache
  { cachedModule :: !CachedModule
  , cachedData   :: !(Map.Map TypeRep Dynamic)
  , isStale      :: !Bool
  } | UriCacheFailed T.Text deriving Show

data CachedModule = CachedModule
  { tcMod          :: !TypecheckedModule
  , locMap         :: !LocMap
  , typeMap        :: !TypeMap
  , moduleMap      :: !ModuleMap
  , revMap         :: !(FilePath -> FilePath)
  , newPosToOld    :: !(Position -> Maybe Position)
  , oldPosToNew    :: !(Position -> Maybe Position)
  }

instance Show CachedModule where
  show CachedModule{} = "CachedModule { .. }"

-- ---------------------------------------------------------------------

-- | Given a list of things with their start and end position in the
-- file, return the set of them that cross include the given position,
-- after it is updated based on edits since the last compile.
getThingsAtPos :: CachedModule -> Position -> [(Position,Position,a)] -> [(Range,a)]
getThingsAtPos cm pos ts =
  case newPosToOld cm pos of
    Nothing   -> []
    Just pos' -> getArtifactsAtPos pos' (genIntervalMap ts)

-- ---------------------------------------------------------------------
-- The following to move into ghc-mod-core

class (Monad m) => HasGhcModuleCache m where
  getModuleCache :: m GhcModuleCache
  setModuleCache :: GhcModuleCache -> m ()

emptyModuleCache :: GhcModuleCache
emptyModuleCache = GhcModuleCache Map.empty Map.empty

data GhcModuleCache = GhcModuleCache
  { cradleCache :: !(Map.Map FilePath GM.Cradle)
              -- ^ map from dirs to cradles
  , uriCaches  :: !UriCaches
  } deriving (Show)

-- ---------------------------------------------------------------------
