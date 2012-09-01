{-# LANGUAGE GADTs, RankNTypes #-}

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
import Control.Monad.Error 
-- from this package
import Control.Monad.Trans.Crtn

-- | input of method of an object with signature s 
data Arg s = forall i o. Arg (s i o) i 

-- | output of methdo of an object with signature s 
data Res s = forall i o.  Res (s i o) o 
           | Ign

{-
-- |
type SObjT s = CrtnT (Arg s) (ResMethodInput s)  

-- | 
type ClientT s = CoroutineT (MethodInput s) (MethodOutput s)
-}

-- | Server object
type SObjT s m = SrvT (Arg s) (Res s) m  
 
-- | Client object
type CObjT s = CliT (Arg s) (Res s)  

-- | Server object base coroutine
type SObjBT s m = CrtnT (Res s) (Arg s) m




-- | convenient error state monad for object  
type EStT s m = ErrorT (CrtnErr ()) (StateT s m)  

-- |  
query :: (Monad m) => CObjT s m r -> EStT (SObjT s m ()) m r  
query cli = do 
  qserv <- lift get 
  (qserv',r) <- mapErrorT lift (qserv <==| cli )
  lift (put qserv')
  return r
  
  


