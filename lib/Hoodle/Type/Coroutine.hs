{-# LANGUAGE GADTs, ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Type.Coroutine 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Type.Coroutine where

-- from other packages 
import Control.Concurrent 
import Control.Monad.Reader 
import Control.Monad.State
import Control.Monad.Trans.Free
import Data.IORef 
-- from hoodle-platform
import Control.Monad.Trans.Crtn 
import Control.Monad.Trans.Crtn.Object
-- from this package
import Hoodle.Type.Event
import Hoodle.Type.XournalState 
-- 

data MainOp i o where 
  Dispatch :: MainOp MyEvent () 


-- | 
-- type MainCoroutine a = CnsmT MyEvent XournalStateIO a 
-- type MainCoroutine a = CrtnT () MyEvent XournalStateIO a 
type MainCoroutine a = SObjBT MainOp XournalStateIO a 

-- |
-- type Driver a = CnsmT MyEvent IO a 
-- type Driver a = SrvT MyEvent () IO a 
type Driver a = SObjT MainOp IO a 

-- type TRef = IORef (Maybe (Driver ()))

type EventVar = MVar (Maybe (Driver ()))

-- | 
{-
dispatch :: MyEvent -> Driver a -> IO (Either a (Driver a))
dispatch ev drv = do 
  x <- runFreeT (runReaderT drv ev) 
  case x of 
    Pure a -> return (Left a)  
    Free (Rqst () next') -> return (Right (ReaderT next'))
-}

dispatch :: (Monad m) => MyEvent -> CObjT MainOp m () 
dispatch ev = do request (Arg Dispatch ev) 
                 return () 


-- | 
nextevent :: MainCoroutine MyEvent 
nextevent = do Arg Dispatch ev <- request (Res Dispatch ())
               return ev 
