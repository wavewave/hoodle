-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Commit 
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Commit where

import Control.Lens (view,set,(.~))
import Control.Monad.Trans
import Control.Monad.State
-- from this package
import Hoodle.Accessor
import Hoodle.Coroutine.Draw 
import Hoodle.GUI.Reflect
import Hoodle.ModelAction.Page
import Hoodle.Type.Coroutine
import Hoodle.Type.Event
import Hoodle.Type.HoodleState 
import Hoodle.Type.Undo
import Hoodle.Util

-- | save state and add the current status in undo history 
commit :: HoodleState -> MainCoroutine () 
commit xstate = do 
  put xstate
  let ui = view gtkUIManager xstate
  liftIO $ reflectUIToggle ui "SAVEA" True
  pureUpdateUhdl $ \uhdl -> 
    let hdlmodst = view hoodleModeState uhdl
        undotable = view undoTable uhdl
        undotable' = addToUndo undotable hdlmodst
    in ((isSaved .~ False) . (undoTable .~ undotable')) uhdl

-- | 
commit_ :: MainCoroutine ()
commit_ = get >>= commit 

-- | 
undo :: MainCoroutine () 
undo = do 
    xstate <- get
    let uhdl = view (unitHoodles.currentUnit) xstate
    let utable = view undoTable uhdl
        cache = view renderCache xstate
    case getPrevUndo utable of 
      Nothing -> msgShout "no undo item yet"
      Just (hdlmodst,newtable) -> do 
        let cid = getCurrentCanvasId uhdl
        callRenderer $ resetHoodleModeStateBuffers cache cid hdlmodst >> return GotNone
        updateUhdl $ \uhdl' -> do
          uhdl'' <- liftIO (updatePageAll hdlmodst uhdl')
          return $ ( (hoodleModeState .~ hdlmodst) . (undoTable .~ newtable)) uhdl''
        invalidateAll 
      
-- |       
redo :: MainCoroutine () 
redo = do 
    xstate <- get
    let uhdl = view (unitHoodles.currentUnit) xstate
        utable = view undoTable uhdl
        cache = view renderCache xstate
        cid = getCurrentCanvasId uhdl
    case getNextUndo utable of 
      Nothing -> msgShout "no redo item"
      Just (hdlmodst,newtable) -> do 
        callRenderer $ resetHoodleModeStateBuffers cache cid hdlmodst >> return GotNone
        updateUhdl $ \uhdl' -> do 
          uhdl'' <- liftIO (updatePageAll hdlmodst uhdl')
          let uhdl''' = ( set hoodleModeState hdlmodst
                       . set undoTable newtable ) uhdl'' 
          return uhdl''' 
        invalidateAll 

-- | 
        
clearUndoHistory :: MainCoroutine () 
clearUndoHistory = pureUpdateUhdl (undoTable .~ (emptyUndo 1))
    




