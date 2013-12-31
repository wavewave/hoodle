{-# LANGUAGE GADTs #-}

module Control.Monad.Trans.Crtn.Logger.Simple where

import Control.Monad.Reader 
-- from this package
import Control.Monad.Trans.Crtn
import Control.Monad.Trans.Crtn.Logger 
import Control.Monad.Trans.Crtn.Object


-- | 
simplelogger :: (MonadLog m) => LogServer m () 
simplelogger = loggerW 0
 
-- |
loggerW :: (MonadLog m) => Int -> LogServer m () 
loggerW num = ReaderT (f num)
  where f n req = 
          case req of 
            Arg WriteLog msg -> do lift (scribe ("log number "++show n++" : "++ msg))
                                   req' <- request (Res WriteLog ())
                                   f (n+1) req' 

