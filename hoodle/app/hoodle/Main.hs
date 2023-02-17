{-# OPTIONS_GHC -w #-}
{-# LANGUAGE NumericUnderscores #-}

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
import Debug.Trace (flushEventLog)
import GHC.Eventlog.Socket (startWait)
import Hoodle.Script (defaultScriptConfig)
import Hoodle.StartUp (hoodleStartMain)

main :: IO ()
main = do
  startWait "/tmp/eventlog.sock"
  forkIO $
    forever $ do
      threadDelay 5_000_000
      flushEventLog
  hoodleStartMain defaultScriptConfig
