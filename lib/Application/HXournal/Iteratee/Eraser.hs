module Application.HXournal.Iteratee.Eraser where

import Graphics.UI.Gtk hiding (get,set,disconnect)
import Application.HXournal.Type.Event
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.XournalState
import Application.HXournal.Device
import Application.HXournal.Draw
import Application.HXournal.Iteratee.EventConnect
import Application.HXournal.Iteratee.Draw
import Application.HXournal.Accessor
import Application.HXournal.ModelAction.Page
import Application.HXournal.ModelAction.Eraser
import Graphics.Xournal.Type
import Graphics.Xournal.Type.Map
import Graphics.Xournal.HitTest
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
              currpage    = getPage cvsInfo
              currlayer = case IM.lookup 0 (pbm_layers currpage) of
                            Nothing -> error "something wrong in eraserProcess"
                            Just l -> l
              
              (newstrokes,maybebbox) = St.runState (eraseHitted hitteststroke) Nothing
              newlayerbbox = currlayer { layerbbox_strokes = newstrokes }    
              newpagebbox = currpage { pbm_layers = IM.adjust (const newlayerbbox) 0 (pbm_layers currpage) } 
              newxojbbox = currxoj { xbm_pages= IM.adjust (const newpagebbox) pgnum (xbm_pages currxoj) }
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
    PenUp _cid' _pcoord -> do 
      disconnect connidmove 
      disconnect connidup 
      invalidateAll
    _ -> return ()
    
