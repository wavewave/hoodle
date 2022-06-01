-- |
-- Module      : Main
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
module Main where

import Hoodle.Script (defaultScriptConfig)
import Hoodle.StartUp (hoodleStartMain)

main :: IO ()
main = hoodleStartMain defaultScriptConfig
