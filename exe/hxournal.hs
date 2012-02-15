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

-- import Application.HXournal.ProgType
-- import Application.HXournal.Command

import Application.HXournal.StartUp

main :: IO () 
main = hxournalStartMain defaultScriptConfig 

-- startUp
-- do 
--  putStrLn "hxournal"
--   param <- cmdArgs mode
--   commandLineProcess param