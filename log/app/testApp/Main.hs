{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -w #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Debug.Trace (flushEventLog, traceEventIO)
import Foreign.Ptr (FunPtr)
import GHC.Eventlog.Socket (startWait)

foreign import ccall safe "cbit.h callTest" c_callTest :: IO ()

type SimpleIO = IO ()

foreign import ccall "wrapper" mkWrapper :: SimpleIO -> IO (FunPtr SimpleIO)

foreign import ccall safe "cbit.h callTest2" c_callTest2 :: FunPtr SimpleIO -> IO ()

simpleAction :: IO ()
simpleAction =
  putStrLn "I am in simple action!"

main :: IO ()
main = do
  startWait "/tmp/eventlog.sock"
  wSimpleAction <- mkWrapper simpleAction
  forkIO $
    forever $ do
      threadDelay 1_000_000
      flushEventLog
  forever $ do
    threadDelay 5_000_000
    putStrLn "am i here"
    for_ [1 .. 10_000] $ \_ ->
      c_callTest2 wSimpleAction
