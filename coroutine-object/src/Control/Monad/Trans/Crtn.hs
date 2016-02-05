{-# LANGUAGE StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, 
             MultiParamTypeClasses, UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Control.Monad.Trans.Crtn
-- Copyright   : (c) 2012-2016 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- definition of coroutine
-- 
-----------------------------------------------------------------------------

module Control.Monad.Trans.Crtn where 

import Control.Monad.Reader 
import Control.Monad.State 
import Control.Monad.Trans.Free

---------------------------
-- general generator 
---------------------------

-- | yield command functor
data Yld o x = Yld o x 

instance Functor (Yld o) where 
  fmap f (Yld o x) = Yld o (f x)

-- | Generator type is single-sided coroutine which only gives an output 
--   without getting any request. 
type GenT o = FreeT (Yld o)

-- | primitive coroutine action yielding an output
yield :: (Monad m) => o -> GenT o m ()  
yield o = wrap (Yld o (return ()))

---------------------------
-- general consumer
---------------------------

-- | await command functor for consumer coroutine
newtype Awt a x = Awt (a->x)

-- | 
instance Functor (Awt g) where 
  fmap f (Awt g) = Awt (f.g)

-- | Consumer type is a single-sided coroutine which only gets an input 
--   without producing output. 
type CnsmT a = FreeT (Awt a) 

-- | primitive coroutine action awaiting an input
await :: (Monad m) => CnsmT a m a
await = wrap (Awt return)

----------------------------
-- general coroutine
----------------------------

-- | command functor of general bidirectional coroutine
data Rqst req ans x = Rqst req (ans -> x)

instance Functor (Rqst req ans) where
  fmap f (Rqst req g) = Rqst req (f.g)

-- | general symmetric bidirectional coroutine
type CrtnT req ans = FreeT (Rqst req ans)

-- | primitive request coroutine  
request :: Monad m => req -> CrtnT req ans m ans
request r = wrap (Rqst r return)

-------------------------------
-- server/client model
------------------------------

-- | Server type 
type SrvT req ans m = ReaderT req (CrtnT ans req m) 

-- | Coroutine type is regarded as a Client type 
--   which can be paired with Server type with opposite request 
--   and answer type.  
type CliT req ans = CrtnT req ans  

-- | type for coroutine status after execution
data CrtnErr = ServerFinished  
             | Other String 

-- | 
deriving instance Show CrtnErr 

-----------------------------
-- communication combinator 
-------------------------------

-- | connecting server and client in error monad
(<==|) :: Monad m => 
          SrvT req ans m r'    -- ^ server coroutine
       -> CliT req ans m r  -- ^ client coroutine
       -> m (Either CrtnErr (SrvT req ans m r', r)) 
s <==| c = do 
    y <- runFreeT c 
    case y of
      Pure r -> return (Right (s,r))
      Free (Rqst rq af) -> do 
        x <- runFreeT (runReaderT s rq) 
        case x of 
          Pure _r' -> return (Left ServerFinished) 
          Free (Rqst ans rf) -> (ReaderT rf) <==| (af ans)

----------------------
-- some utility 
------------------------

-- | combine state and free monad with base state monad transformer 
--   with a base monad m to free monad with the base monad m
--   Think this as fusing down the state monad  
mapStateDown :: (Monad m, Functor f) => 
                s -> FreeT f (StateT s m) a -> FreeT f m a 
mapStateDown st m =
    FreeT $ do x <- flip runStateT st $ runFreeT m 
               case x of 
                 (Pure r,_) -> return (Pure r) 
                 (Free f,st') -> 
                   return . Free . fmap (mapStateDown st') $ f
