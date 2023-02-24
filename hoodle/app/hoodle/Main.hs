{-# LANGUAGE CPP #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

#ifdef USE_EVENTLOG
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Debug.Trace (flushEventLog)
import GHC.Eventlog.Socket (startWait)
#endif
import Hoodle.Script (defaultScriptConfig)
import Hoodle.StartUp (hoodleStartMain)

main :: IO ()
main = do
#ifdef USE_EVENTLOG
  startWait "/tmp/eventlog.sock"
  _ <- forkIO $
    forever $ do
      threadDelay 500_000
      flushEventLog
#endif
  hoodleStartMain defaultScriptConfig
