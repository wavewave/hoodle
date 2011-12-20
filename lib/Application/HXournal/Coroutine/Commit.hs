module Application.HXournal.Coroutine.Commit where

import Application.HXournal.Type.XournalState 
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Event
import Application.HXournal.ModelAction.File

import Data.Label
import Control.Monad.Trans
import Application.HXournal.Accessor

commit :: HXournalState -> Iteratee MyEvent XournalStateIO ()
commit xstate = do 
  let ui = get gtkUIManager xstate
  liftIO $ toggleSave ui True
  let xstate' = set isSaved False xstate
  putSt xstate' 
