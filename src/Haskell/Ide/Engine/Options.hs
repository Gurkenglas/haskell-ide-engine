{-# LANGUAGE CPP #-}
module Haskell.Ide.Engine.Options where

#if __GLASGOW_HASKELL__ < 804
import           Data.Semigroup             hiding (option)
#endif
import           Options.Applicative.Simple

data GlobalOpts = GlobalOpts
  { optDebugOn       :: Bool
  , optLogFile       :: Maybe String
  , optLsp           :: Bool
  , optJson          :: Bool
  , projectRoot      :: Maybe String
  , optGhcModVomit   :: Bool
  , optEkg           :: Bool
  , optEkgPort       :: Int
  , optCaptureFile   :: Maybe FilePath
  , optExamplePlugin :: Bool
  } deriving (Show)

globalOptsParser :: Parser GlobalOpts
globalOptsParser = GlobalOpts
  <$> switch
       ( long "debug"
      <> short 'd'
      <> help "Generate debug output"
       )
  <*> optional (strOption
       ( long "logfile"
      <> short 'l'
      <> metavar "LOGFILE"
      <> help "File to log to, defaults to stdout"
       ))
  <*> flag True True
       ( long "lsp"
       <> help "Enable the Language Server Protocol transport on STDIO (default)")
  <*> switch
       ( long "json"
       <> help "Enable JSON transport on STDIO")
  <*> optional (strOption
       ( long "project-root"
      <> short 'r'
      <> metavar "PROJECTROOT"
      <> help "Root directory of project, defaults to cwd"))
  <*> switch
       ( long "vomit"
       <> help "enable vomit logging for ghc-mod")
  <*> switch
       ( long "ekg"
       <> help "enable ekg collection and display on http://localhost:8000")
  <*> option auto
       ( long "port"
      <> short 'p'
      <> metavar "PORT"
      <> help "TCP port to use for EKG server. Only used if --ekg is set. Default 8000"
      <> value 8000
       )
  <*> optional (strOption
       ( long "capture"
      <> short 'c'
      <> metavar "CAPTUREFILE"
      <> help "File to capture the session to"
       ))
  <*> switch
       ( long "example"
       <> help "Enable Example2 plugin. Useful for developers only")
