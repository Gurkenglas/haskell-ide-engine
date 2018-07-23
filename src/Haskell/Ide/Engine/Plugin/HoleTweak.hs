{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Haskell.Ide.Engine.Plugin.Example2 where

import           Control.Monad.IO.Class
#if __GLASGOW_HASKELL__ < 804
import           Data.Monoid
#endif
import qualified Data.Text                     as T
import           Haskell.Ide.Engine.MonadTypes

-- ---------------------------------------------------------------------

example2Descriptor :: PluginDescriptor
example2Descriptor = PluginDescriptor
  {
    pluginName = "Hello World"
  , pluginDesc = "An example of writing an HIE plugin"
  , pluginCommands =
      [ PluginCommand "sayHello" "say hello" sayHelloCmd
      , PluginCommand "sayHelloTo ""say hello to the passed in param" sayHelloToCmd
      ]
  }

-- ---------------------------------------------------------------------

sayHelloCmd :: CommandFunc () T.Text
sayHelloCmd = CmdSync $ \_ -> return (IdeResultOk sayHello)

sayHelloToCmd :: CommandFunc T.Text T.Text
sayHelloToCmd = CmdSync $ \n -> do
  r <- liftIO $ sayHelloTo n
  return $ IdeResultOk r

-- ---------------------------------------------------------------------

sayHello :: T.Text
sayHello = "hello from ExamplePlugin2"

sayHelloTo :: T.Text -> IO T.Text
sayHelloTo n = return $ "hello " <> n <> " from ExamplePlugin2"



import Control.Monad.Search
import Control.Lens

repair :: MonadSearch c m => Pretext' (->) Snippet File -> m File
repair pretext = do
  guard $ typechecks $ peek [| _ |] pretext -- Can changing this snippet fix the file?
  asum
     $  repair <$> alaf Compose experiment holes pretext -- Is it enough to change some subsnippet?
    <|> runPretext pretext <$> strategies -- Try some actual changes.

strategies :: MonadSearch c m => [Snippet -> m Snippet]
strategies =
  [ return . apply [| (_ .) |]
  , return . apply [| (. _) |]
  ]

-- Applies a function to an argument.
apply :: Snippet -> Snippet -> Snippet

-- Asks the compiler if a file is fine.
typechecks :: File -> Bool
