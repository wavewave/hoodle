module Application.HXournal.Type where

import Control.Monad.Coroutine 
import Control.Monad.Coroutine.SuspensionFunctors
import Data.Functor.Identity (Identity(..))
import Control.Monad.State
import Data.Sequence

import Data.IORef

import Text.Xournal.Type

import Graphics.UI.Gtk

type Trampoline m x = Coroutine Identity m x 
type Generator a m x = Coroutine (Yield a) m x
type Iteratee a m x = Coroutine (Await a) m x

type XournalStateIO = StateT XournalState IO 

data PenDrawing = PenDrawing { penDrawingPoints :: Seq (Double,Double)
                             } 

data XournalState = 
  XournalState 
  { xoj :: Xournal 
  , wdw :: Button
  , darea :: DrawingArea
  , currpage :: Int 
  , currpendrawing :: PenDrawing 
  , x_tref :: IORef (Await MyEvent (Iteratee MyEvent XournalStateIO ()))
  , x_sref :: IORef (XournalState)
  } 
                      

data MyEvent = ButtonLeft 
             | ButtonRight 
             | ButtonRefresh 
             | ButtonQuit 
             | UpdateCanvas
             | PenDown (Double,Double)
             | PenMove (Double,Double)
             | PenUp   (Double,Double)
             deriving (Show,Eq,Ord)


emptyXournalState :: XournalState
emptyXournalState = 
  XournalState 
  { xoj = emptyXournal
  , wdw = undefined
  , darea = undefined
  , currpage = 0 
  , currpendrawing = PenDrawing empty 
  } 