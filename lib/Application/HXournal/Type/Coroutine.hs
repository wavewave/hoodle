module Application.HXournal.Type.Coroutine where

import Data.IORef 
import Application.HXournal.Type.Event
import Application.HXournal.Type.XournalState 
import Control.Monad.Coroutine 
import Control.Monad.Coroutine.SuspensionFunctors
import Data.Functor.Identity (Identity(..))

type Trampoline m x = Coroutine Identity m x 
type Generator a m x = Coroutine (Yield a) m x
type Iteratee a m x = Coroutine (Await a) m x

type SusAwait =  Await MyEvent (Iteratee MyEvent XournalStateIO ())
type TRef = IORef SusAwait 
type SRef = IORef HXournalState

type MainCoroutine a = Iteratee MyEvent XournalStateIO a 
