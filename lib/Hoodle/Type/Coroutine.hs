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
import Control.Monad.Trans.Crtn 
import Hoodle.Type.Event
import Hoodle.Type.XournalState 

-- | 
type MainCoroutine a = CnsmT MyEvent XournalStateIO a 

-- |
type Driver a = CnsmT MyEvent IO a 

-- | 
mapDown :: HoodleState -> MainCoroutine a -> Driver a 
mapDown st m = 
    FreeT $ do x <- flip runStateT st $ runFreeT m 
               case x of 
                 (Pure r,_) -> return (Pure r) 
                 (Free (Awt next),st') -> 
                   return . Free . Awt $ \ev -> mapDown st' (next ev)
                      

type TRef = IORef (Maybe (MyEvent -> Driver ()))

