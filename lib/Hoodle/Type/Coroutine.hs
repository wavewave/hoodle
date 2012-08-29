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

import Control.Monad.State
import Control.Monad.Trans.Free
import Data.IORef 
-- from this package
import Control.Monad.Coroutine 
import Hoodle.Type.Event
import Hoodle.Type.XournalState 

-- | 
type MainCoroutine a = Consumer MyEvent XournalStateIO a 

-- |
type Driver a = Consumer MyEvent IO a 

-- | 
mapDown :: HoodleState -> MainCoroutine a -> Driver a 
mapDown st m = 
    FreeT $ do x <- flip runStateT st $ runFreeT m 
               case x of 
                 (Pure r,_) -> return (Pure r) 
                 (Free (Await next),st') -> 
                   return . Free . Await $ \ev -> mapDown st' (next ev)
                      
{-                      
                      do 
                    x <- runFreeT m 
                    case x of               
                      Pure r -> undefined -- return (Left r) 
                      Free (Await next) -> return next 
    in FreeT (Await (\ev -> mapDown st' (next ev)))
                                
  evalStateT m st -}

type TRef = IORef (Maybe (MyEvent -> Driver ()))
            -- (MyEvent -> MainCoroutine ()) 
-- type SRef = IORef HoodleState

