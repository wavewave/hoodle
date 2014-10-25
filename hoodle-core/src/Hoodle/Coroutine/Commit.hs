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
      Nothing -> liftIO $ putStrLn "no undo item yet"
      Just (hdlmodst1,newtable) -> do 
        hdlmodst <- liftIO $ resetHoodleModeStateBuffers cache hdlmodst1
        updateUhdl $ \uhdl -> do
          uhdl' <- liftIO (updatePageAll hdlmodst uhdl)
          return $ ( (hoodleModeState .~ hdlmodst) . (undoTable .~ newtable)) uhdl'
        invalidateAll 
      
-- |       
redo :: MainCoroutine () 
redo = do 
    xstate <- get
    let uhdl = view (unitHoodles.currentUnit) xstate
        utable = view undoTable uhdl
        cache = view renderCache xstate
    case getNextUndo utable of 
      Nothing -> liftIO $ putStrLn "no redo item"
      Just (hdlmodst1,newtable) -> do 
        hdlmodst <- liftIO $ resetHoodleModeStateBuffers cache hdlmodst1
        updateUhdl $ \uhdl -> do 
          uhdl' <- liftIO (updatePageAll hdlmodst uhdl)
          let uhdl'' = ( set hoodleModeState hdlmodst
                       . set undoTable newtable ) uhdl' 
          return uhdl'' 
        -- modify (set unitHoodles (putTheUnit uhdl''))
        invalidateAll 

-- | 
        
clearUndoHistory :: MainCoroutine () 
clearUndoHistory = -- do 
    -- xstate <- get
    -- let uhdl = view (unitHoodles.currentUnit) xstate
    -- modify (set unitHoodles (putTheUnit (set undoTable (emptyUndo 1) uhdl)))
    pureUpdateUhdl (undoTable .~ (emptyUndo 1))
    




