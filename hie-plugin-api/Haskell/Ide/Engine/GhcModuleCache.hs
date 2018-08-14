{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Haskell.Ide.Engine.GhcModuleCache where

import           Control.Concurrent.MVar.Lifted
import           Control.Lens.TH
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Dynamic (Dynamic)
import           Data.Typeable (TypeRep)

import qualified GhcMod.Types                      as GM

import           GHC                               (TypecheckedModule)

import Haskell.Ide.Engine.ArtifactMap

import Language.Haskell.LSP.Types

type UriCaches = Map.Map FilePath (MVar UriCache)
type UriCache = Either T.Text UriCacheFound

data UriCacheFound = UriCacheFound
  { _cachedModule :: !CachedModule
  , _cachedData   :: !(Map.Map TypeRep Dynamic)
  , _isStale      :: !Bool
  }

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

data GhcModuleCache = GhcModuleCache
  { _cradleCache :: !(Map.Map FilePath GM.Cradle)
              -- ^ map from dirs to cradles
  , _uriCaches  :: !UriCaches
  }
makeLenses ''GhcModuleCache
makeLenses ''UriCacheFound
-- ---------------------------------------------------------------------
