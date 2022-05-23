module Hoodle.Coroutine.Commit where

import Control.Lens (set, view, (.~))
import Control.Monad.State
import Control.Monad.Trans
import Hoodle.Accessor
import Hoodle.Coroutine.Draw
import Hoodle.GUI.Reflect
import Hoodle.ModelAction.Page
import Hoodle.Type.Coroutine
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
  let uhdl = view (unitHoodles . currentUnit) xstate
  let utable = view undoTable uhdl
  case getPrevUndo utable of
    Nothing -> msgShout "no undo item yet"
    Just (hdlmodst, newtable) -> do
      let cid = getCurrentCanvasId uhdl
      callRenderer_ $ resetHoodleModeStateBuffers cid hdlmodst
      updateUhdl $ \uhdl' -> do
        uhdl'' <- liftIO (updatePageAll hdlmodst uhdl')
        return $ ((hoodleModeState .~ hdlmodst) . (undoTable .~ newtable)) uhdl''
      invalidateAll

-- |
redo :: MainCoroutine ()
redo = do
  xstate <- get
  let uhdl = view (unitHoodles . currentUnit) xstate
      utable = view undoTable uhdl
      cid = getCurrentCanvasId uhdl
  case getNextUndo utable of
    Nothing -> msgShout "no redo item"
    Just (hdlmodst, newtable) -> do
      callRenderer_ $ resetHoodleModeStateBuffers cid hdlmodst
      updateUhdl $ \uhdl' -> do
        uhdl'' <- liftIO (updatePageAll hdlmodst uhdl')
        let uhdl''' =
              ( set hoodleModeState hdlmodst
                  . set undoTable newtable
              )
                uhdl''
        return uhdl'''
      invalidateAll

-- |
clearUndoHistory :: MainCoroutine ()
clearUndoHistory = pureUpdateUhdl (undoTable .~ emptyUndo 1)
