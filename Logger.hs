{-# LANGUAGE GADTs, FlexibleInstances, UndecidableInstances, OverlappingInstances #-}

---------------------------
-- | describe logger
---------------------------

module Logger where

import Control.Monad.Trans 
--
import Coroutine 
import Object

-------------------------
-- Logging monad 
-------------------------

class (Monad m) => MonadLog m where 
    scribe :: String -> m () 

instance (MonadTrans t, MonadLog m, Monad (t m)) => MonadLog (t m) where 
  scribe = lift . scribe  
  
instance MonadLog IO where
    scribe = putStrLn 

{-
instance (MonadIO m) => MonadLog m where
    scribe = liftIO . putStrLn 
-}

data LogOp i o where 
  WriteLog :: LogOp String () 


type LogInput = MethodInput LogOp 

type LogServer m r = ServerObj LogOp m r 

type LogClient m r = ClientObj LogOp m r

-- | 
writeLog :: (Monad m) => String -> LogClient m () 
writeLog msg = do request (Input WriteLog msg) 
                  return () 

-- | 
logger :: (MonadLog m) => LogServer m () 
logger = loggerW 0
 
-- |
loggerW :: (MonadLog m) => Int -> LogServer m () 
loggerW n = Server (f n)
  where f n req = case req of 
                    Input WriteLog msg -> do lift (scribe (show n ++ " : " ++ msg))
                                             req <- request (Output WriteLog ())
                                             f (n+1) req 








