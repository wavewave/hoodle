{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, GADTs, Rank2Types #-}

module Main where

import Control.Concurrent 
-- from this package 
-- import Control.Monad.Coroutine
import Control.Monad.Coroutine.Driver 
import Control.Monad.Coroutine.Event 
import Control.Monad.Coroutine.EventHandler 
-- import Control.Monad.Coroutine.Logger.Simple 
import Control.Monad.Coroutine.Logger.WebLog
import Control.Monad.Coroutine.World.CmdExec


-- |

test_tickingevent :: IO () 
test_tickingevent = do 
    dref <- newEmptyMVar :: IO (MVar (Driver IO ()))
    let logger = weblogger "http://127.0.0.1:7800"
    putMVar dref (driver logger world (eventHandler dref))
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
    {- if n == 5 
      then eventHandler mvar Start 
      else if n `mod` 3 == 0 
           then eventHandler mvar Render
           else eventHandler mvar (Message ("test : " ++ show n)) -}
    let action | n `mod` 10 == 5 = eventHandler mvar Start  
               | n `mod` 10 == 9 = eventHandler mvar (Init (n `div` 10))
               | otherwise = eventHandler mvar Render 
    action 

   

    putStrLn "_-_-_-_-_-_-_-_-_-_-_-_-_-"
    threadDelay (1*second)
    ticking mvar (n+1)

------------------------------- 
-- test 
-------------------------------

main :: IO ()
main = test_tickingevent 
