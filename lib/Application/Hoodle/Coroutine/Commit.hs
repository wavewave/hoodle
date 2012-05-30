-----------------------------------------------------------------------------
-- |
-- Module      : Application.Hoodle.Coroutine.Commit 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Application.Hoodle.Coroutine.Commit where

import Application.Hoodle.Type.XournalState 
import Application.Hoodle.Type.Coroutine
import Application.Hoodle.Type.Undo 
import Application.Hoodle.Coroutine.Draw 
import Application.Hoodle.ModelAction.File
import Application.Hoodle.ModelAction.Page
import Data.Label
import Control.Monad.Trans
import Application.Hoodle.Accessor

-- | save state and add the current status in undo history 

commit :: HoodleState -> MainCoroutine () 
commit xstate = do 
  let ui = get gtkUIManager xstate
  liftIO $ toggleSave ui True
  let xojstate = get xournalstate xstate
      undotable = get undoTable xstate 
      undotable' = addToUndo undotable xojstate
      xstate' = set isSaved False 
                . set undoTable undotable'
                $ xstate
  putSt xstate' 

-- | 
  
commit_ :: MainCoroutine ()
commit_ = getSt >>= commit 

-- | 

undo :: MainCoroutine () 
undo = do 
    xstate <- getSt
    let utable = get undoTable xstate
    case getPrevUndo utable of 
      Nothing -> liftIO $ putStrLn "no undo item yet"
      Just (xojstate1,newtable) -> do 
        xojstate <- liftIO $ resetXournalStateBuffers xojstate1 
        putSt . set xournalstate xojstate
              . set undoTable newtable 
              =<< (liftIO (updatePageAll xojstate xstate))
        invalidateAll 
      
-- |       
  
redo :: MainCoroutine () 
redo = do 
    xstate <- getSt
    let utable = get undoTable xstate
    case getNextUndo utable of 
      Nothing -> liftIO $ putStrLn "no redo item"
      Just (xojstate1,newtable) -> do 
        xojstate <- liftIO $ resetXournalStateBuffers xojstate1         
        putSt . set xournalstate xojstate
              . set undoTable newtable 
              =<< (liftIO (updatePageAll xojstate xstate))
        invalidateAll 

-- | 
        
clearUndoHistory :: MainCoroutine () 
clearUndoHistory = do 
    xstate <- getSt
    putSt . set undoTable (emptyUndo 1) $ xstate
    




