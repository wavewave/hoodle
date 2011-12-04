module Application.HXournal.Iteratee.Eraser where

import Graphics.UI.Gtk hiding (get,set,disconnect)

import Application.HXournal.Type.Event
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.XournalState

import Application.HXournal.Type.Event
import Application.HXournal.Device
import Application.HXournal.Draw

import Application.HXournal.Iteratee.EventConnect
import Application.HXournal.Iteratee.Draw

import Application.HXournal.Accessor

import Application.HXournal.ModelAction.Page
import Application.HXournal.ModelAction.Eraser

import Graphics.Xournal.Type
import Graphics.Xournal.Render.BBox
import Graphics.Xournal.HitTest



import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.Trans
import qualified Control.Monad.State as St

import Control.Category
import Data.Label
import qualified Data.Map as M

import Prelude hiding ((.), id)

import Text.Xournal.Type

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
    PenMove cid' pcoord -> do 
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
              pages       = xournalPages currxoj 
              currpage    = getPage cvsInfo
              pagesbefore = take pgnum pages 
              pagesafter  = drop (pgnum+1) pages 
              currlayer   = head (pageLayers currpage) 
              otherlayers = tail (pageLayers currpage) 
              (newstrokes,maybebbox) = St.runState (eraseHitted hitteststroke) Nothing
              newlayerbbox = currlayer { layerbbox_strokes = newstrokes }    
              newpagebbox = currpage 
                              { pagebbox_layers = newlayerbbox : otherlayers } 
              newxojbbox = currxoj { xojbbox_pages = pagesbefore
                                                     ++ [newpagebbox]
                                                     ++ pagesafter } 
              newxojstate = ViewAppendState newxojbbox
              xstate' = set xournalstate newxojstate 
                        . updatePageAll newxojstate $ xstate 
          lift $ St.put xstate' 
          case maybebbox of 
            Just bbox -> invalidateDrawBBox cid bbox
            Nothing -> return ()
          newstrs <- getAllStrokeBBoxInCurrentPage
          eraserProcess cid cpg connidup connidmove newstrs (x,y)
        else eraserProcess cid cpg connidmove connidup strs (x,y) 
    PenUp cid' _pcoord -> do 
      disconnect connidmove 
      disconnect connidup 
      invalidateAll
    _ -> return ()
    
