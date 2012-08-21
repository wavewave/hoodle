{-# LANGUAGE GADTs, FlexibleInstances, UndecidableInstances, OverlappingInstances #-}

---------------------------
-- | describe logger
---------------------------

module Control.Monad.Coroutine.Logger where

import Control.Monad.Reader
--
import Control.Monad.Coroutine 
import Control.Monad.Coroutine.Object

-------------------------
-- Logging monad 
-------------------------

class (Monad m) => MonadLog m where 
    scribe :: String -> m () 

instance (MonadTrans t, MonadLog m, Monad (t m)) => MonadLog (t m) where 
  scribe = lift . scribe  
  
instance MonadLog IO where
    scribe = putStrLn 


data LogOp i o where 
  WriteLog :: LogOp String () 


type LogInput = MethodInput LogOp 

type LogServer m r = ServerObj LogOp m r 

type LogClient m r = ClientObj LogOp m r

-- | 
writeLog :: (Monad m) => String -> LogClient m () 
writeLog msg = do request (Input WriteLog msg) 
                  return () 









