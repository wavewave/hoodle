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

import Data.IORef 
import Hoodle.Type.Event
import Hoodle.Type.XournalState 
import Control.Monad.Coroutine 
-- import Control.Monad.Coroutine.SuspensionFunctors
import Data.Functor.Identity (Identity(..))

-- type Trampoline m x = Coroutine Identity m x 
-- type Generator a m x = Coroutine (Yield a) m x
-- type Iteratee a m x = Coroutine (Await a) m x

-- type SusAwait =  Await MyEvent (Iteratee MyEvent XournalStateIO ())


type MainCoroutine a = Consumer MyEvent XournalStateIO a 

type TRef = IORef (MyEvent -> MainCoroutine ()) -- SusAwait
type SRef = IORef HoodleState