{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, GADTs, Rank2Types #-}

module Main where

import Control.Concurrent 
-- import Control.Concurrent.MVar 
-- import Control.Monad.State
-- import Control.Monad.Trans.Error
-- import System.INotify 
-- from this package 
-- import Control.Monad.Coroutine
import Control.Monad.Coroutine.Driver 
import Control.Monad.Coroutine.Event 
import Control.Monad.Coroutine.EventHandler 
import Control.Monad.Coroutine.World.Sample2
-- import Control.Monad.Coroutine.Object 
-- import FileObserver 
-- import HINotify 
-- import Lsof 
-- import QServer


-- |

test_tickingevent :: IO () 
test_tickingevent = do 
    dref <- newEmptyMVar :: IO (MVar (Driver IO ())) -- newMVar (undefined :: Driver IO ()) 
    -- _ <- takeMVar dref  
    putMVar dref (driver world (eventHandler dref))
    putStrLn "starting ticking" 
    ticking dref 0    


second :: Int 
second = 1000000

-- | 

ticking :: MVar (Driver IO ()) -> Int -> IO () 
ticking mvar n = do 
    putStrLn "--------------------------"
    putStrLn ("ticking : " ++ show n)
    {- if n `mod` 10 == 0 
      then eventHandler mvar Open  
      else if n `mod` 10 == 5                 
        then eventHandler mvar Close 
        else if n `mod` 10 == 3
          then eventHandler mvar Render
          else eventHandler mvar (Message ("test : " ++ show n)) -}
    if n == 5 
      then eventHandler mvar Start 
      else if n `mod` 3 == 0 
           then eventHandler mvar Render
           else eventHandler mvar (Message ("test : " ++ show n))

    putStrLn "_-_-_-_-_-_-_-_-_-_-_-_-_-"
    threadDelay (1*second)
    ticking mvar (n+1)

------------------------------- 
-- test 
-------------------------------

main :: IO ()
main = test_tickingevent 
