{-# LANGUAGE JavaScriptFFI #-}
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar,takeMVar,newEmptyMVar)
import Control.Monad (void)
import GHCJS.Foreign.Callback (Callback, OnBlocked(ThrowWouldBlock), syncCallback)

foreign import javascript unsafe "callback = $1"
  js_set_callback :: Callback a -> IO ()

test :: IO ()
test = do
  putStrLn "test function is called."

main :: IO ()
main = do
  putStrLn "ghcjs started"
  callback <- syncCallback ThrowWouldBlock test

  js_set_callback callback


  threadDelay 10000000
{-
  v <- newEmptyMVar :: IO (MVar ())
  void $ forkIO $ do
    takeMVar v
-}
