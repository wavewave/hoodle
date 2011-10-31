module Application.HXournal.Type where

import Control.Monad.Coroutine 
import Control.Monad.Coroutine.SuspensionFunctors
import Data.Functor.Identity (Identity(..))
import Control.Monad.State

import Text.Xournal.Type

import Graphics.UI.Gtk

type Trampoline m x = Coroutine Identity m x 
type Generator a m x = Coroutine (Yield a) m x
type Iteratee a m x = Coroutine (Await a) m x

type XournalStateIO = StateT XournalState IO 

data XournalState = XournalState { xoj :: Xournal 
                                 , wdw :: Button
                                 , darea :: DrawingArea
                                 , currpage :: Int } 
--                    deriving Show
                      

data MyEvent = ButtonLeft | ButtonRight | ButtonRefresh | ButtonQuit | UpdateCanvas
             deriving (Show,Eq,Ord)


emptyXournalState :: XournalState
emptyXournalState = XournalState { xoj = emptyXournal, wdw = undefined, darea = undefined, currpage = 0 } 