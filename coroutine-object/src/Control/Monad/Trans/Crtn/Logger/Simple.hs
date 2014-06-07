{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
loggerW :: forall m. (MonadLog m) => Int -> LogServer m () 
loggerW num = ReaderT (f num)
  where
    f :: Int -> LogInput -> CrtnT (Res LogOp) (Arg LogOp) m () 
    f n (Arg WriteLog msg) = do 
      lift (scribe ("log number "++show n++" : "++ msg))
      req' <- request (Res WriteLog ())
      f (n+1) req' 

