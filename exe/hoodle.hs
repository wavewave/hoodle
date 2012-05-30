-----------------------------------------------------------------------------
-- |
-- Module      : Main
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Main where

-- import System.Console.CmdArgs

-- import Application.Hoodle.ProgType
-- import Application.Hoodle.Command

import Application.Hoodle.Script 
import Application.Hoodle.StartUp

main :: IO () 
main = hoodleStartMain defaultScriptConfig 

-- startUp
-- do 
--  putStrLn "hoodle"
--   param <- cmdArgs mode
--   commandLineProcess param