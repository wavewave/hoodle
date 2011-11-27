module Application.HXournal.Iteratee.Highlighter where

import Graphics.UI.Gtk

import Application.HXournal.Device 
import Application.HXournal.Type.Event
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.XournalState
import Application.HXournal.Type.Event

import Control.Monad.Trans

highlighterStart :: PointerCoord -> Iteratee MyEvent XournalStateIO ()
highlighterStart _pcoord = do 
  liftIO $ putStrLn "highlighter started"

