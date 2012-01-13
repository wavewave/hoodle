module Application.HXournal.Coroutine.Eraser where

import Graphics.UI.Gtk hiding (get,set,disconnect)
import Application.HXournal.Type.Event
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.XournalState
import Application.HXournal.Device
import Application.HXournal.Draw
import Application.HXournal.Coroutine.EventConnect
import Application.HXournal.Coroutine.Draw
import Application.HXournal.Coroutine.Commit
import Application.HXournal.Accessor
import Application.HXournal.ModelAction.Page
import Application.HXournal.ModelAction.Eraser

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

eraserStart :: CanvasId 
               -> PointerCoord 
               -> Iteratee MyEvent XournalStateIO ()
eraserStart cid pcoord = do 
    xstate <- changeCurrentCanvasId cid 
    let cvsInfo = getCanvasInfo cid xstate
        zmode = get (zoomMode.viewInfo) cvsInfo
    geometry <- getCanvasGeometry cvsInfo 
    let (x,y) = device2pageCoord geometry zmode pcoord 
    connidup   <- connectPenUp cvsInfo     
    connidmove <- connectPenMove cvsInfo   
    strs <- getAllStrokeBBoxInCurrentPage
    eraserProcess cid geometry connidup connidmove strs (x,y)
  
eraserProcess :: CanvasId
              -> CanvasPageGeometry
              -> ConnectId DrawingArea -> ConnectId DrawingArea 
              -> [StrokeBBox] 
              -> (Double,Double)
              -> Iteratee MyEvent XournalStateIO ()
eraserProcess cid cpg connidmove connidup strs (x0,y0) = do 
  r <- await 
  xstate <- getSt
  let cvsInfo = getCanvasInfo cid xstate 
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

              -- case IM.lookup 0 (glayers currpage) of
              --              Nothing -> error "something wrong in eraserProcess"
              --               Just l -> l
              (newstrokes,maybebbox1) = St.runState (eraseHitted hitteststroke) Nothing
              maybebbox = fmap (flip inflate 2.0) maybebbox1
          newlayerbbox <- liftIO . updateLayerBuf maybebbox . set g_bstrokes newstrokes $ currlayer 
          let newpagebbox = adjustCurrentLayer newlayerbbox currpage 
                -- currpage { glayers = IM.adjust (const newlayerbbox) 0 (glayers currpage) } 
              newxojbbox = currxoj { gpages= IM.adjust (const newpagebbox) pgnum (gpages currxoj) }
              newxojstate = ViewAppendState newxojbbox
          commit . set xournalstate newxojstate 
                 . updatePageAll newxojstate $ xstate 
          invalidateWithBufInBBox maybebbox cid 
          newstrs <- getAllStrokeBBoxInCurrentPage
          eraserProcess cid cpg connidup connidmove newstrs (x,y)
        else eraserProcess cid cpg connidmove connidup strs (x,y) 
    PenUp _cid' _pcoord -> do 
      disconnect connidmove 
      disconnect connidup 
      invalidateAll
    _ -> return ()
    
