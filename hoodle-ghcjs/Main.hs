{-# LANGUAGE JavaScriptFFI #-}
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar,takeMVar,newEmptyMVar)
import Control.Monad (forever, void)
import GHCJS.Foreign.Callback (Callback, OnBlocked(ContinueAsync, ThrowWouldBlock), asyncCallback, syncCallback)
import System.IO (hPutStrLn, hFlush, stdout)

foreign import javascript unsafe "callback = $1"
  js_set_callback :: Callback a -> IO ()

test :: IO ()
test = do
  hPutStrLn stdout "test function is called."
  hFlush stdout

main :: IO ()
main = do
  putStrLn "ghcjs started"
  callback <- syncCallback ThrowWouldBlock test
  js_set_callback callback

