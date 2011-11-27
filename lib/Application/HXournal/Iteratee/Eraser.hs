module Application.HXournal.Iteratee.Eraser where

import Graphics.UI.Gtk hiding (get,set)

import Application.HXournal.Type.Event
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.XournalState
import Application.HXournal.Type.XournalBBox
import Application.HXournal.Type.Event
import Application.HXournal.Device
import Application.HXournal.HitTest
import Application.HXournal.Util.AlterList
import Application.HXournal.Draw

import Application.HXournal.Iteratee.EventConnect
import Application.HXournal.Iteratee.Draw

import Application.HXournal.Accessor

import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.Trans
import qualified Control.Monad.State as St

import Control.Category
import Data.Label
import qualified Data.Map as M

import Prelude hiding ((.), id)

import Text.Xournal.Type

eraserStart :: PointerCoord -> Iteratee MyEvent XournalStateIO ()
eraserStart pcoord = do 
  liftIO $ putStrLn "eraser started"
  xstate <- lift St.get 
  let currCvsId = get currentCanvas xstate 
      maybeCurrCvs = M.lookup currCvsId (get canvasInfoMap xstate)
  case maybeCurrCvs of 
    Nothing -> return ()
    Just currCvsInfo -> do  
      let canvas = get drawArea currCvsInfo
          xojbbox = get xournalbbox xstate
          pagenum = get currentPageNum currCvsInfo
          page = (!!pagenum) . xournalPages $ xojbbox
          zmode = get (zoomMode.viewInfo) currCvsInfo
          (x0,y0) = get (viewPortOrigin.viewInfo) currCvsInfo
      geometry <- liftIO (getCanvasPageGeometry canvas page (x0,y0))
      let (x,y) = device2pageCoord geometry zmode pcoord 
      connidup <- connPenUp canvas currCvsId
      connidmove <- connPenMove canvas currCvsId
      strs <- getAllStrokeBBoxInCurrentPage
      eraserProcess geometry connidup connidmove strs (x,y)
  
eraserProcess :: CanvasPageGeometry
              -> ConnectId DrawingArea -> ConnectId DrawingArea 
              -> [StrokeBBox] 
              -> (Double,Double)
              -> Iteratee MyEvent XournalStateIO ()
eraserProcess cpg connidmove connidup strs (x0,y0) = do 
  r <- await 
  xstate <- lift St.get 
  let currCvsId = get currentCanvas xstate 
      maybeCurrCvs = M.lookup currCvsId (get canvasInfoMap xstate)
  case maybeCurrCvs of 
    Nothing -> return ()
    Just currCvsInfo -> do   
      case r of 
        PenMove cid pcoord -> do 
          let zmode  = get (zoomMode.viewInfo) currCvsInfo
              (x,y) = device2pageCoord cpg zmode pcoord 
              line = ((x0,y0),(x,y))
              hittestbbox = mkHitTestBBox line strs   
              (hitteststroke,hitState) = 
                St.runState (hitTestStrokes line hittestbbox) False
          if hitState 
            then do 
              let currxoj     = get xournalbbox xstate 
                  pgnum       = get currentPageNum currCvsInfo
                  pages       = xournalPages currxoj 
                  currpage    = pages !! pgnum
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
              lift $ St.put (set xournalbbox newxojbbox xstate)
              case maybebbox of 
                Just bbox -> invalidateBBox currCvsId bbox
                Nothing -> return ()
              newstrs <- getAllStrokeBBoxInCurrentPage
              eraserProcess cpg connidup connidmove newstrs (x,y)
            else       
              eraserProcess cpg connidmove connidup strs (x,y) 
        PenUp cid _pcoord -> do 
          liftIO $ signalDisconnect connidmove 
          liftIO $ signalDisconnect connidup 
          invalidate currCvsId
        _ -> return ()
    
eraseHitted :: AlterList NotHitted (AlterList NotHitted Hitted) 
               -> St.State (Maybe BBox) [StrokeBBox]
eraseHitted Empty = error "something wrong in eraseHitted"
eraseHitted (n :-Empty) = return (unNotHitted n)
eraseHitted (n:-h:-rest) = do 
  mid <- elimHitted h 
  return . (unNotHitted n ++) . (mid ++) =<< eraseHitted rest
