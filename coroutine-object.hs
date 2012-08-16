{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, GADTs, Rank2Types #-}

module Main where

import Control.Monad.State
import Control.Monad.Trans.Error
-- 
import QServer 

-- | test initializer 
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






------------------------------- 
-- test 
-------------------------------

main :: IO ()
main = do 
    test_qclient
