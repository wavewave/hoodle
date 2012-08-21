{-# LANGUAGE GADTs #-}

module Control.Monad.Coroutine.Logger.Simple where

import Control.Monad.Reader 
-- from this package
import Control.Monad.Coroutine
import Control.Monad.Coroutine.Logger 
import Control.Monad.Coroutine.Object


-- | 
logger :: (MonadLog m) => LogServer m () 
logger = loggerW 0
 
-- |
loggerW :: (MonadLog m) => Int -> LogServer m () 
loggerW num = ReaderT (f num)
  where f n req = case req of 
                    Input WriteLog msg -> do lift (scribe ("log number " ++ show n ++ " : " ++ msg))
                                             req' <- request (Output WriteLog ())
                                             f (n+1) req' 
