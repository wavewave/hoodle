module Application.HXournal.Iteratee.Highlighter where

import Application.HXournal.Device 
import Application.HXournal.Type.Event
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.XournalState
import Control.Monad.Trans

highlighterStart :: PointerCoord -> Iteratee MyEvent XournalStateIO ()
highlighterStart _pcoord = do 
  liftIO $ putStrLn "highlighter started"

