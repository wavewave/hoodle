-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.Coroutine.Eraser 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--

module Application.HXournal.Coroutine.Eraser where

import Graphics.UI.Gtk hiding (get,set,disconnect)
import Application.HXournal.Type.Event
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.XournalState
import Application.HXournal.Device
import Application.HXournal.View.Draw
import Application.HXournal.Coroutine.EventConnect
import Application.HXournal.Coroutine.Draw
import Application.HXournal.Coroutine.Commit
import Application.HXournal.Accessor
import Application.HXournal.ModelAction.Page
import Application.HXournal.ModelAction.Eraser
import Application.HXournal.ModelAction.Layer

import Data.Xournal.Generic

import Data.Xournal.BBox
import Graphics.Xournal.Render.HitTest
import Graphics.Xournal.Render.BBox
import Graphics.Xournal.Render.BBoxMapPDF
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.Trans
import qualified Control.Monad.State as St
import Control.Category
import Data.Label
import qualified Data.IntMap as IM
import Prelude hiding ((.), id)
import Application.HXournal.Coroutine.Pen 

eraserStart :: CanvasId 
               -> PointerCoord 
               -> MainCoroutine () 
eraserStart cid = commonPenStart eraserAction cid  
  where eraserAction _cinfo cpg _zmode (cidup,cidmove) (x,y) = do 
          strs <- getAllStrokeBBoxInCurrentLayer
          eraserProcess cid cpg cidup cidmove strs (x,y)

eraserProcess :: CanvasId
              -> CanvasPageGeometry
              -> ConnectId DrawingArea -> ConnectId DrawingArea 
              -> [StrokeBBox] 
              -> (Double,Double)
              -> MainCoroutine () 
eraserProcess cid cpg connidmove connidup strs (x0,y0) = do 
    r <- await 
    xst <- getSt
    selectBoxAction (fsingle r xst) (error "eraserProcess") . getCanvasInfo cid $ xst 
  where 
    fsingle r xstate cvsInfo = do
      case r of 
        PenMove _cid' pcoord -> do 
          let zmode  = get (zoomMode.viewInfo) cvsInfo
              (x,y) = device2pageCoord cpg zmode pcoord 
              line = ((x0,y0),(x,y))
              hittestbbox = mkHitTestBBox line strs   
              (hitteststroke,hitState) = 
                St.runState (hitTestStrokes line hittestbbox) False
          if hitState 
            then do 
              let currxoj     = unView . get xournalstate $ xstate 
                  pgnum       = get currentPageNum cvsInfo
                  (mcurrlayer, currpage) = getCurrentLayerOrSet . getPage $ cvsInfo
                  currlayer = maybe (error "eraserProcess") id mcurrlayer
              let (newstrokes,maybebbox1) = St.runState (eraseHitted hitteststroke) Nothing
                  maybebbox = fmap (flip inflate 2.0) maybebbox1
              newlayerbbox <- liftIO . updateLayerBuf maybebbox . set g_bstrokes newstrokes $ currlayer 
              let newpagebbox = adjustCurrentLayer newlayerbbox currpage 
                  newxojbbox = currxoj { gpages= IM.adjust (const newpagebbox) pgnum (gpages currxoj) }
                  newxojstate = ViewAppendState newxojbbox
              commit . set xournalstate newxojstate 
                     . updatePageAll newxojstate $ xstate 
              invalidateWithBuf cid 
              newstrs <- getAllStrokeBBoxInCurrentLayer
              eraserProcess cid cpg connidup connidmove newstrs (x,y)
            else eraserProcess cid cpg connidmove connidup strs (x,y) 
        PenUp _cid' _pcoord -> do 
          disconnect connidmove 
          disconnect connidup 
          invalidateAll
        _ -> return ()
    

