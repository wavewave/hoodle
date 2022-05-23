{-# LANGUAGE CPP #-}

module Hoodle.Script where

import Config.Dyre.Relaunch
import Hoodle.Script.Hook

-- |
data ScriptConfig = ScriptConfig
  { message :: Maybe String,
    hook :: Maybe Hook,
    errorMsg :: Maybe String
  }

-- |
defaultScriptConfig :: ScriptConfig
defaultScriptConfig = ScriptConfig Nothing Nothing Nothing

-- |
showError :: ScriptConfig -> String -> ScriptConfig
showError cfg msg = cfg {errorMsg = Just msg}

-- |
relaunchApplication :: IO ()
relaunchApplication = do
  putStrLn "relaunching hoodle!"
  relaunchMaster Nothing
