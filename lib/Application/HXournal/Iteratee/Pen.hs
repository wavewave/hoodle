module Application.HXournal.Iteratee.Pen where

import Graphics.UI.Gtk hiding (get,set)

import Application.HXournal.Device 
import Application.HXournal.Type.Event
import Application.HXournal.Type.Enum
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.XournalState

import Application.HXournal.Iteratee.Draw
import Application.HXournal.Iteratee.EventConnect
import Application.HXournal.Accessor
import Application.HXournal.ModelAction.Pen

import Application.HXournal.Draw

import Control.Monad.Trans
import qualified Control.Monad.State as St
import Control.Monad.Coroutine.SuspensionFunctors

import Data.Sequence hiding (filter)
import qualified Data.Map as M
import Data.Maybe 

import Control.Category
import Data.Label
import Prelude hiding ((.), id)

import Text.Xournal.Type 
import Graphics.Xournal.Render.BBox

penStart :: CanvasId -> PointerCoord -> Iteratee MyEvent XournalStateIO ()
penStart cid pcoord = do 
    xstate1 <- getSt 
    let xstate = set currentCanvas cid xstate1     
    putSt xstate
    let maybeCvs = M.lookup cid (get canvasInfoMap xstate)
    case maybeCvs of 
      Nothing -> error "penStart wrong"
      Just cvsInfo -> do 
        let currxoj = get xournalbbox xstate        
            canvas = get drawArea cvsInfo   
            pagenum = get currentPageNum cvsInfo
            page = (!!pagenum) . xournalPages $ currxoj
            (x0,y0) = get (viewPortOrigin.viewInfo) cvsInfo
            pinfo = get penInfo xstate
            zmode = get (zoomMode.viewInfo) cvsInfo
        geometry <- liftIO (getCanvasPageGeometry canvas page (x0,y0) )
        let (x,y) = device2pageCoord geometry zmode pcoord 
        connidup <- connPenUp canvas cid      
        connidmove <- connPenMove canvas cid
        pdraw <-penProcess cid geometry connidmove connidup 
                           (empty |> (x,y)) (x,y) 
        let (newxoj,bbox) = addPDraw pinfo currxoj pagenum pdraw
            bbox' = inflate bbox (get (penWidth.penInfo) xstate) 
            xstate' = set xournalbbox newxoj $ xstate
        putSt xstate'
        -- invalidateOther
        let cinfoMap = get canvasInfoMap xstate'
            keys = M.keys cinfoMap 
        mapM_ (flip invalidateInBBox bbox') (filter (/=cid) keys) 
        
        return ()

penProcess :: CanvasId
           -> CanvasPageGeometry
           -> ConnectId DrawingArea -> ConnectId DrawingArea 
           -> Seq (Double,Double) -> (Double,Double) 
           -> Iteratee MyEvent XournalStateIO (Seq (Double,Double))
penProcess cid cpg connidmove connidup pdraw (x0,y0) = do 
  r <- await 
  xstate <- lift St.get
  let -- currCvsId = get currentCanvas xstate 
      maybeCvs = M.lookup cid (get canvasInfoMap xstate)
  case maybeCvs of 
    Nothing -> error "something wrong" 
    Just cvsInfo -> do 
      case r of 
        PenMove cid' pcoord -> do 
          let canvas = get drawArea cvsInfo
              zmode  = get (zoomMode.viewInfo) cvsInfo
              pcolor = get (penColor.penInfo) xstate 
              pwidth = get (penWidth.penInfo) xstate 
              (x,y) = device2pageCoord cpg zmode pcoord 
              pcolRGBA = fromJust (M.lookup pcolor penColorRGBAmap) 
          liftIO $ drawSegment canvas cpg zmode pwidth pcolRGBA (x0,y0) (x,y)
          penProcess cid cpg connidmove connidup (pdraw |> (x,y)) (x,y) 
        PenUp cid' pcoord -> do 
          let zmode = get (zoomMode.viewInfo) cvsInfo
              (x,y) = device2pageCoord cpg zmode pcoord 
          liftIO $ signalDisconnect connidmove
          liftIO $ signalDisconnect connidup
          return (pdraw |> (x,y)) 
        other -> do
          -- defaultEventProcess other        
          penProcess cid cpg connidmove connidup pdraw (x0,y0) 
