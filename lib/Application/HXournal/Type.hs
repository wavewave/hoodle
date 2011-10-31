module Application.HXournal.Type where

import Control.Monad.Coroutine 
import Control.Monad.Coroutine.SuspensionFunctors
import Data.Functor.Identity (Identity(..))
import Control.Monad.State

type Trampoline m x = Coroutine Identity m x 
type Generator a m x = Coroutine (Yield a) m x
type Iteratee a m x = Coroutine (Await a) m x

type XournalStateIO = StateT XournalState IO 

type XournalState = Int

data MyEvent = ButtonLeft | ButtonRight | ButtonRefresh | ButtonQuit
             deriving (Show,Eq,Ord)
