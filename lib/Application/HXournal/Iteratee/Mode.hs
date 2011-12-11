module Application.HXournal.Iteratee.Mode where

import Application.HXournal.Type.Event
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.XournalState
import Application.HXournal.Accessor

import Graphics.Xournal.Type.Map
import Graphics.Xournal.Type.Select

import Control.Monad.Trans

import Control.Applicative
import Control.Category
import Data.Label
import Prelude hiding ((.),id)



modeChange :: MyEvent -> Iteratee MyEvent XournalStateIO ()
modeChange ToViewAppendMode = do 
  xstate <- getSt
  let xojstate = get xournalstate xstate
  case xojstate of 
    ViewAppendState _ -> return () 
    SelectState txoj -> do 
      liftIO $ putStrLn "to view append mode"
      putSt 
        . set xournalstate (ViewAppendState (XournalBBoxMap <$> tx_pages $ txoj))
        $ xstate  
modeChange ToSelectMode = do 
  xstate <- getSt
  let xojstate = get xournalstate xstate
  case xojstate of 
    ViewAppendState xoj -> do 
      liftIO $ putStrLn "to select mode"
      putSt
        . set xournalstate (SelectState (tempXournalSelectFromXournalBBoxMap xoj))
        $ xstate  
    SelectState _ -> return ()
modeChange _ = return ()
