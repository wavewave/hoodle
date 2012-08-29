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

import System.Console.CmdArgs
import Hoodle.ProgType
import Hoodle.Command

import Control.Monad
import Control.Concurrent

import qualified Config.Dyre as Dyre 
import Config.Dyre.Relaunch

import System.FilePath
import System.Environment

import Hoodle.Script

-- | 

hoodleMain ScriptConfig {..} = do 
    case errorMsg of 
      Nothing -> return () 
      Just em -> putStrLn $ "Error: " ++ em 
    -- 
    maybe (return ()) putStrLn message   
    -- 
    param <- cmdArgs mode
    commandLineProcess param hook

-- | 
    
hoodleStartMain = Dyre.wrapMain $ Dyre.defaultParams 
  { Dyre.projectName = "start"
  , Dyre.configDir = Just dirHoodled
  , Dyre.realMain = hoodleMain 
  , Dyre.showError = showError 
  -- , Dyre.hidePackages = ["meta-hoodle"] 
  } 

-- | 

dirHoodled :: IO FilePath 
dirHoodled = do
  homedir <- getEnv "HOME"
  return (homedir </> ".hoodle.d")


