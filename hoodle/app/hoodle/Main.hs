{-# OPTIONS_GHC -w #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import GHC.Eventlog.Socket (flush, startWait)
import Hoodle.Script (defaultScriptConfig)
import Hoodle.StartUp (hoodleStartMain)
import System.Mem (performGC)

main :: IO ()
main = do
  startWait "/tmp/eventlog.sock"
  forkIO $
    forever $ do
      threadDelay 1000000
      flush
  {- forkIO $
    forever $ do
      threadDelay 10000000
      performGC -}
  hoodleStartMain defaultScriptConfig
