{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Crtn.Logger where

import Control.Monad (void)
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Crtn (request)
import Control.Monad.Trans.Crtn.Object
  ( Arg (..),
    CObjT,
    SObjT,
  )

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

type LogInput = Arg LogOp

type LogServer m r = SObjT LogOp m r

type LogClient m r = CObjT LogOp m r

-- |
writeLog :: (Monad m) => String -> LogClient m ()
writeLog msg =
  void $ request (Arg WriteLog msg)
