module Application.HXournal.Iteratee.Eraser where

import Graphics.UI.Gtk hiding (get,set)

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
    xstate1 <- getSt
    let xstate = set currentCanvas cid xstate1 
    putSt xstate
    let maybeCvs = M.lookup cid (get canvasInfoMap xstate)
    case maybeCvs of 
      Nothing -> return ()
      Just cvsInfo -> do  
        let canvas = get drawArea cvsInfo
            page = getPage cvsInfo
            -- xojbbox = unView . get xournalstate $ xstate
            
            
            -- pagenum = get currentPageNum cvsInfo
            -- page = (!!pagenum) . xournalPages $ xojbbox
            zmode = get (zoomMode.viewInfo) cvsInfo
            (x0,y0) = get (viewPortOrigin.viewInfo) cvsInfo
        geometry <- liftIO (getCanvasPageGeometry canvas page (x0,y0))
        let (x,y) = device2pageCoord geometry zmode pcoord 
        connidup <- connPenUp canvas cid
        connidmove <- connPenMove canvas cid
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
  xstate <- lift St.get 
  let  cinfoMap = get canvasInfoMap xstate
  let  maybeCvs = M.lookup cid cinfoMap
  case maybeCvs of 
    Nothing -> return ()
    Just cvsInfo -> do   
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
                  -- currpage    = pages !! pgnum
                  currpage = getPage cvsInfo
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
                  cvsInfo' = updatePage newxojstate cvsInfo
                  cinfoMap' = M.adjust (const cvsInfo') cid cinfoMap
                  xstate' = set xournalstate newxojstate
                          . set canvasInfoMap cinfoMap'
                          $ xstate
              lift $ St.put xstate' 
              case maybebbox of 
                Just bbox -> invalidateDrawBBox cid bbox
                Nothing -> return ()

              newstrs <- getAllStrokeBBoxInCurrentPage
              eraserProcess cid cpg connidup connidmove newstrs (x,y)
            else       
              eraserProcess cid cpg connidmove connidup strs (x,y) 
        PenUp cid' _pcoord -> do 
          liftIO $ signalDisconnect connidmove 
          liftIO $ signalDisconnect connidup 
          invalidateAll
        _ -> return ()
    
eraseHitted :: AlterList NotHitted (AlterList NotHitted Hitted) 
               -> St.State (Maybe BBox) [StrokeBBox]
eraseHitted Empty = error "something wrong in eraseHitted"
eraseHitted (n :-Empty) = return (unNotHitted n)
eraseHitted (n:-h:-rest) = do 
  mid <- elimHitted h 
  return . (unNotHitted n ++) . (mid ++) =<< eraseHitted rest
