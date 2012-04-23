-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.Script
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Application.HXournal.Script where 

import Application.HXournal.Script.Hook
import Config.Dyre.Relaunch

-- | 

data ScriptConfig = ScriptConfig { message :: Maybe String 
                                 , hook :: Maybe Hook
                                 , errorMsg :: Maybe String 
                                 }  

-- | 

defaultScriptConfig :: ScriptConfig 
defaultScriptConfig = ScriptConfig Nothing Nothing Nothing

-- | 

showError :: ScriptConfig -> String -> ScriptConfig
showError cfg msg = cfg { errorMsg = Just msg } 


-- | 

relaunchApplication :: IO ()
relaunchApplication = do 
  putStrLn "relaunching hxournal!"
  relaunchMaster Nothing 
  
  