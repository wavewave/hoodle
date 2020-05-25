{-# LANGUAGE InterruptibleFFI #-}

module Main where

import Asterius.Types (JSFunction(..))
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, replicateM_, void)
import Data.IORef (newIORef, readIORef, writeIORef)
import Foreign.StablePtr (StablePtr)

foreign import javascript "wrapper" makeHaskellCallback :: IO () -> IO JSFunction

foreign import javascript "callback = $1" register :: JSFunction -> IO ()

tester :: IO ()
tester = do
  ref <- newIORef (0::Int)
  putStrLn "tester"
  replicateM_ 10 $ do
    threadDelay 100000
    n <- readIORef ref
    putStrLn $ "step: " ++ show n
    writeIORef ref (n+1)

main :: IO ()
main = do
  jsf <- makeHaskellCallback tester
  register jsf
