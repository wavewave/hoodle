
-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.Coroutine.Commit 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
module Application.HXournal.Coroutine.Commit where

import Application.HXournal.Type.XournalState 
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Event
import Application.HXournal.Type.Undo 

import Application.HXournal.Coroutine.Draw 
import Application.HXournal.ModelAction.File
import Application.HXournal.ModelAction.Page

import Data.Label
-- import Control.Monad (liftM)
import Control.Monad.Trans
import Application.HXournal.Accessor



commit :: HXournalState -> MainCoroutine () 
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

undo :: MainCoroutine () 
undo = do 
    liftIO $ putStrLn "undo is called"
    xstate <- getSt
    let utable = get undoTable xstate
    case getPrevUndo utable of 
      Nothing -> liftIO $ putStrLn "no undo item yet"
      Just (xojstate1,newtable) -> do 
        xojstate <- liftIO $ resetXournalStateBuffers xojstate1 
        let xstate' = set xournalstate xojstate
                      . set undoTable newtable 
                      . updatePageAll xojstate 
                      $ xstate 
        putSt xstate'
        invalidateAll 
      
  
redo :: MainCoroutine () 
redo = do 
    liftIO $ putStrLn "redo is called"
    xstate <- getSt
    let utable = get undoTable xstate
    case getNextUndo utable of 
      Nothing -> liftIO $ putStrLn "no redo item"
      Just (xojstate1,newtable) -> do 
        xojstate <- liftIO $ resetXournalStateBuffers xojstate1         
        let xstate' = set xournalstate xojstate
                      . set undoTable newtable 
                      . updatePageAll xojstate 
                      $ xstate 
        putSt xstate'
        invalidateAll 

clearUndoHistory :: MainCoroutine () 
clearUndoHistory = do 
    liftIO $ putStrLn "clearUndoHistory is called"
    xstate <- getSt
    putSt . set undoTable (emptyUndo 1) $ xstate
    




