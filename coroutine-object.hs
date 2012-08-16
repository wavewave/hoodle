{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, GADTs, Rank2Types #-}

module Main where

import Control.Concurrent.MVar 
import Control.Monad.State
import Control.Monad.Trans.Error
-- from this package 
import Driver 
import EventHandler 
import QServer

-- | test qclient 
test_qclient :: IO () 
test_qclient = eaction >>= either putStrLn (const (putStrLn "good end")) 
  where eaction :: IO (Either String ())
        eaction = flip evalStateT qserver $ runErrorT $ do 
          r2 <- query $ do addQ (1 :: Int) 
                           addQ 2
                           addQ 3 
                           x <- retrieveQ  
                           y <- retrieveQ
                           return (x,y)
          liftIO $ print r2 
          r3  <- query $ do z <- retrieveQ 
                            return z 
          liftIO $ print r3  

-- | test driver 
test_driver :: IO () 
test_driver = eaction >>= either putStrLn (const (putStrLn "good end"))
  where eaction :: IO (Either String ())
        eaction = flip evalStateT driver $ runErrorT $ do 
          query $ dispatch (Message "hello")
          query $ dispatch (Message "how are you?")
          return () 
          
-- | test event handling 
test_eventhandler :: IO () 
test_eventhandler = do 
  dref <- newMVar driver 
  eventHandler dref (Message "hi")
  putStrLn "----"
  eventHandler dref (Message "hello")
  putStrLn "----"
  eventHandler dref (Message "dlsl") 
  putStrLn "----"
  





------------------------------- 
-- test 
-------------------------------

main :: IO ()
main = do 
    test_qclient
    test_driver 
    test_eventhandler