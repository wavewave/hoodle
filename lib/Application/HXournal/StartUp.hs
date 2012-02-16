{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.StartUp
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Application.HXournal.StartUp where 

import System.Console.CmdArgs
import Application.HXournal.ProgType
import Application.HXournal.Command

import Control.Monad
import Control.Concurrent

import qualified Config.Dyre as Dyre 
import Config.Dyre.Relaunch

import System.FilePath
import System.Environment

import Application.HXournal.Script

-- | 

hxournalMain ScriptConfig {..} = do 
    case errorMsg of 
      Nothing -> return () 
      Just em -> putStrLn $ "Error: " ++ em 
  
    param <- cmdArgs mode
    commandLineProcess param hook

-- | 
    
hxournalStartMain = Dyre.wrapMain $ Dyre.defaultParams 
  { Dyre.projectName = "start"
  , Dyre.configDir = Just dirHXournald
  , Dyre.realMain = hxournalMain 
  , Dyre.showError = showError 
  , Dyre.hidePackages = ["meta-hxournal"] 
  } 

-- | 

dirHXournald :: IO FilePath 
dirHXournald = do
  homedir <- getEnv "HOME"
  return (homedir </> ".hxournal.d")


{-
-- | main starting point of the whole program

startUp :: IO () 
startUp = do 
    -- putStrLn "welcome to hxournal"
    -- param <- cmdArgs mode
    -- commandLineProcess param 
    hxournalStartMain defaultScriptConfig  
-}