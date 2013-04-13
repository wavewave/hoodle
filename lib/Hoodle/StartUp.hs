{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.StartUp
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.StartUp where 

-- from other packages 
import qualified Config.Dyre as Dyre 
import           System.Console.CmdArgs
import           System.FilePath
import           System.Environment
-- from hoodle-platform 
import           Hoodle.Script
-- from this package
import           Hoodle.ProgType
import           Hoodle.Command

-- | 
hoodleMain :: ScriptConfig -> IO ()
hoodleMain ScriptConfig {..} = do 
    case errorMsg of 
      Nothing -> return () 
      Just em -> putStrLn $ "Error: " ++ em 
    maybe (return ()) putStrLn message   
    param <- cmdArgs mode
    commandLineProcess param hook

-- | 
hoodleStartMain :: ScriptConfig -> IO ()
hoodleStartMain = Dyre.wrapMain $ Dyre.defaultParams 
  { Dyre.projectName = "start"
  , Dyre.configDir = Just dirHoodled
  , Dyre.realMain = hoodleMain 
  , Dyre.showError = showError 
  , Dyre.ghcOpts = [ "-threaded" ]
  } 

-- | 
dirHoodled :: IO FilePath 
dirHoodled = do
  homedir <- getEnv "HOME"
  return (homedir </> ".hoodle.d")


