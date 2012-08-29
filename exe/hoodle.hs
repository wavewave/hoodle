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

import Hoodle.Script 
import Hoodle.StartUp

main :: IO () 
main = hoodleStartMain defaultScriptConfig 

