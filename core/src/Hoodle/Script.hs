{-# LANGUAGE CPP #-}

module Hoodle.Script where 

import Hoodle.Script.Hook
#ifdef DYRE
import Config.Dyre.Relaunch
#endif

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
#ifdef DYRE
relaunchApplication :: IO ()
relaunchApplication = do 
  putStrLn "relaunching hoodle!"
  relaunchMaster Nothing 
#endif
  
