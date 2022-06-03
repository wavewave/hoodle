module Plugin.CheckImports (plugin) where

import GhcPlugins
  ( CommandLineOption,
    CoreM,
    CoreToDo,
    Plugin (..),
    defaultPlugin,
    putMsgS,
  )

plugin :: Plugin
plugin =
  defaultPlugin
    { installCoreToDos = install
    }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  putMsgS "Hello!"
  pure todo
