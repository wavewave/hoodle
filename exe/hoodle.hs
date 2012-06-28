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

-- import Hoodle.ProgType
-- import Hoodle.Command

import Hoodle.Script 
import Hoodle.StartUp

main :: IO () 
main = hoodleStartMain defaultScriptConfig 

-- startUp
-- do 
--  putStrLn "hoodle"
--   param <- cmdArgs mode
--   commandLineProcess param