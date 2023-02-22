{-# OPTIONS_GHC -w #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Debug.Trace (flushEventLog)
import GHC.Eventlog.Socket (startWait)
import Hoodle.Script (defaultScriptConfig)
import Hoodle.StartUp (hoodleStartMain)

main :: IO ()
main = do
#ifdef USE_EVENTLOG
  startWait "/tmp/eventlog.sock"
  forkIO $
    forever $ do
      threadDelay 5_000_000
      flushEventLog
#endif
  hoodleStartMain defaultScriptConfig
