{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Control.Monad.Trans.Crtn.Object 
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- coroutine that can be interpreted as an object
-- 
-----------------------------------------------------------------------------

module Control.Monad.Trans.Crtn.Object where 

-- from other packages
import Control.Monad.State 
-- import Control.Monad.Error 
import Control.Monad.Trans.Either 
-- from this package
import Control.Monad.Trans.Crtn

-- | input of method of an object with signature s 
data Arg s = forall i o. Arg (s i o) i 

-- | output of methdo of an object with signature s 
data Res s = forall i o.  Res (s i o) o 
           | Ign


-- | Server object
type SObjT s m = SrvT (Arg s) (Res s) m  
 
-- | Client object
type CObjT s = CliT (Arg s) (Res s)  

-- | Server object base coroutine
type SObjBT s m = CrtnT (Res s) (Arg s) m




-- | convenient error state monad for object  
type EStT s m = EitherT CrtnErr (StateT s m)  

-- |  
query :: forall m s r. (Monad m) => CObjT s m r -> EStT (SObjT s m ()) m r  
query cli = do 
  qserv <- lift get 
  let result :: m (Either CrtnErr (SrvT (Arg s) (Res s) m (), r))
      result = qserv <==| cli
      r2 :: StateT (SObjT s m ()) m (Either CrtnErr (SrvT (Arg s) (Res s) m (), r))
      r2 = lift result
  (qserv',r) <- EitherT r2 
  lift (put qserv')
  return r
  
  


