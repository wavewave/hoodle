{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : Hoodle.StartUp
-- Copyright   : (c) 2012, 2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
module Hoodle.StartUp where

import qualified Config.Dyre as Dyre
import Hoodle.Command
import Hoodle.ProgType
import Hoodle.Script
import System.Console.CmdArgs
import System.Environment
import System.FilePath

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
hoodleStartMain =
  Dyre.wrapMain $
    Dyre.newParams "start" hoodleMain showError

-- |
dirHoodled :: IO FilePath
dirHoodled = do
  homedir <- getEnv "HOME"
  return (homedir </> ".hoodle.d")
