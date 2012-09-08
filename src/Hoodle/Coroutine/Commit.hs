-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Commit 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Commit where

-- import Data.Label
import Control.Lens
import Control.Monad.Trans
import Control.Monad.State
-- from this package
import Hoodle.Coroutine.Draw 
import Hoodle.ModelAction.File
import Hoodle.ModelAction.Page
import Hoodle.Type.Coroutine
import Hoodle.Type.HoodleState 
import Hoodle.Type.Undo 

-- | save state and add the current status in undo history 

commit :: HoodleState -> MainCoroutine () 
commit xstate = do 
  let ui = view gtkUIManager xstate
  liftIO $ toggleSave ui True
  let hdlmodst = view hoodleModeState xstate
      undotable = view undoTable xstate 
      undotable' = addToUndo undotable hdlmodst
      xstate' = set isSaved False 
                . set undoTable undotable'
                $ xstate
  put xstate' 

-- | 
  
commit_ :: MainCoroutine ()
commit_ = get >>= commit 

-- | 

undo :: MainCoroutine () 
undo = do 
    xstate <- get
    let utable = view undoTable xstate
    case getPrevUndo utable of 
      Nothing -> liftIO $ putStrLn "no undo item yet"
      Just (hdlmodst1,newtable) -> do 
        hdlmodst <- liftIO $ resetHoodleModeStateBuffers hdlmodst1 
        put . set hoodleModeState hdlmodst
            . set undoTable newtable 
            =<< (liftIO (updatePageAll hdlmodst xstate))
        invalidateAll 
      
-- |       
  
redo :: MainCoroutine () 
redo = do 
    xstate <- get
    let utable = view undoTable xstate
    case getNextUndo utable of 
      Nothing -> liftIO $ putStrLn "no redo item"
      Just (hdlmodst1,newtable) -> do 
        hdlmodst <- liftIO $ resetHoodleModeStateBuffers hdlmodst1         
        put . set hoodleModeState hdlmodst
            . set undoTable newtable 
            =<< (liftIO (updatePageAll hdlmodst xstate))
        invalidateAll 

-- | 
        
clearUndoHistory :: MainCoroutine () 
clearUndoHistory = do 
    xstate <- get
    put . set undoTable (emptyUndo 1) $ xstate
    




