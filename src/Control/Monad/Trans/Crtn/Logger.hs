{-# LANGUAGE GADTs, FlexibleInstances, UndecidableInstances, OverlappingInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Control.Monad.Trans.Crtn.Logger 
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- describe logger
--
-----------------------------------------------------------------------------

module Control.Monad.Trans.Crtn.Logger where

import Control.Monad.Reader
--
import Control.Monad.Trans.Crtn 
import Control.Monad.Trans.Crtn.Object

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
writeLog msg = do request (Arg WriteLog msg) 
                  return () 









