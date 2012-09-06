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
import           Control.Concurrent 
import           Control.Monad.Error 
import           Control.Monad.Reader 
import           Control.Monad.State
import           Control.Monad.Trans.Free
-- import           Data.IORef 
-- from hoodle-platform
import           Control.Monad.Trans.Crtn 
import           Control.Monad.Trans.Crtn.Object
import qualified Control.Monad.Trans.Crtn.Driver as D
import           Control.Monad.Trans.Crtn.World
-- from this package
import           Hoodle.Type.Event
import           Hoodle.Type.XournalState 
-- 

-- |
data MainOp i o where 
  DoEvent :: MainOp MyEvent () 

doEvent :: (Monad m) => MyEvent -> CObjT MainOp m () 
doEvent ev = request (Arg DoEvent ev) >> return ()

-- |
type MainCoroutine = MainObjB 
                     
type MainObjB = SObjBT MainOp (StateT HoodleState WorldObjB)

-- | 
type MainObj = SObjT MainOp (StateT HoodleState WorldObjB)

-- | 
nextevent :: MainCoroutine MyEvent 
nextevent = do Arg DoEvent ev <- request (Res DoEvent ())
               return ev 

-- | 
type WorldObj = SObjT (WorldOp MyEvent DriverB) DriverB  

-- | 
type WorldObjB = SObjBT (WorldOp MyEvent DriverB) DriverB 

-- | 
world :: HoodleState -> MainObj () -> WorldObj ()
world xstate initmc = ReaderT staction  
  where 
    staction req = runStateT (go initmc req) xstate >> return ()
    go :: MainObj () 
          -> Arg (WorldOp MyEvent DriverB) 
          -> StateT HoodleState WorldObjB () 
    go mcobj (Arg GiveEvent ev) = do 
      Right mcobj' <- runErrorT $ liftM fst (mcobj <==| doEvent ev)
      req <- lift $ request (Res GiveEvent ())
      go mcobj' req  
    go mcobj (Arg FlushLog _) = do  
      req <- lift $ request Ign
      go mcobj req 
    go mcobj (Arg FlushQueue _) = do 
      req <- lift $ request Ign
      go mcobj req 




-- | 
-- type MainCoroutine a = CnsmT MyEvent XournalStateIO a 
-- type MainCoroutine a = CrtnT () MyEvent XournalStateIO a 

-- |
-- type Driver a = CnsmT MyEvent IO a 
-- type Driver a = SrvT MyEvent () IO a 
type Driver a = D.Driver MyEvent IO a -- SObjT MainOp IO a 

type DriverB = SObjBT (D.DrvOp MyEvent) IO  

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

{-
dispatch :: (Monad m) => MyEvent -> CObjT MainOp m () 
dispatch ev = do request (Arg Dispatch ev) 
                 return () 
-}



