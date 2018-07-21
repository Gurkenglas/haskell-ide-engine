+{-# LANGUAGE CPP               #-}
+{-# LANGUAGE OverloadedStrings #-}
+module Haskell.Ide.Engine.Plugin.Example2 where
+
+import           Control.Monad.IO.Class
+#if __GLASGOW_HASKELL__ < 804
+import           Data.Monoid
+#endif
+import qualified Data.Text                     as T
+import           Haskell.Ide.Engine.MonadTypes
+import           Haskell.Ide.Engine.PluginUtils
+import           Haskell.Ide.Engine.Plugin.HaRe (HarePointWithText(..))
+
+-- ---------------------------------------------------------------------
+
+example2Descriptor :: PluginDescriptor
+example2Descriptor = PluginDescriptor
+  {
+    pluginName = "Hello World"
+  , pluginDesc = "An example of writing an HIE plugin"
+  , pluginCommands =
+      [ PluginCommand "sayHello"   "say hello"                        sayHelloCmd
+      , PluginCommand "sayHelloTo" "say hello to the passed in param" sayHelloToCmd
+      , PluginCommand "addPragma" "Return an edit to add a pragma at the top of the file" addPragmaCmd
+      ]
+  }
+
+-- ---------------------------------------------------------------------
+
+sayHelloCmd :: CommandFunc () T.Text
+sayHelloCmd = CmdSync $ \_ -> return (IdeResultOk sayHello)
+
+sayHelloToCmd :: CommandFunc T.Text T.Text
+sayHelloToCmd = CmdSync $ \n -> do
+  r <- liftIO $ sayHelloTo n
+  return $ IdeResultOk r
+
+-- ---------------------------------------------------------------------
+
+sayHello :: T.Text
+sayHello = "hello from ExamplePlugin2"
+
+sayHelloTo :: T.Text -> IO T.Text
+sayHelloTo n = return $ "hello " <> n <> " from ExamplePlugin2"
+
+-- ---------------------------------------------------------------------
+
+addPragmaCmd :: CommandFunc HarePointWithText WorkspaceEdit
+addPragmaCmd = CmdSync $ \(HPT uri _pos pragmaName) -> do
+   liftToGhc $ addPragmaCmd' uri pragmaName
+
+addPragmaCmd' :: Uri -> T.Text -> IdeM (IdeResult WorkspaceEdit)
+addPragmaCmd' uri pragmaName = do
+  diff <-  diffText (uri, "")
+                    ("{-# LANGUAGE " <> pragmaName <> " #-}\n")
+                    IncludeDeletions
+  return $ IdeResultOk diff
