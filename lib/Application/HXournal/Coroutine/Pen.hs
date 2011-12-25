module Application.HXournal.Coroutine.Pen where

import Graphics.UI.Gtk hiding (get,set,disconnect)
import Application.HXournal.Device 
import Application.HXournal.Type.Event
import Application.HXournal.Type.Enum
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.XournalState
import Application.HXournal.Coroutine.Draw
import Application.HXournal.Coroutine.EventConnect
import Application.HXournal.Coroutine.Commit
import Application.HXournal.Accessor
import Application.HXournal.ModelAction.Pen
import Application.HXournal.ModelAction.Page
import Application.HXournal.Draw
import Control.Monad.Trans

import Control.Monad.Coroutine.SuspensionFunctors
import Data.Sequence hiding (filter)
import qualified Data.Map as M
import Data.Maybe 
import Control.Category
import Data.Label
import Prelude hiding ((.), id)
import Graphics.Xournal.Render.BBox


penStart :: CanvasId -> PointerCoord -> Iteratee MyEvent XournalStateIO ()
penStart cid pcoord = do 
    xstate <- changeCurrentCanvasId cid 
    let cvsInfo = getCanvasInfo cid xstate 
    let currxoj = unView . get xournalstate $ xstate        
        pagenum = get currentPageNum cvsInfo
        pinfo = get penInfo xstate
        zmode = get (zoomMode.viewInfo) cvsInfo
    geometry <- getCanvasGeometry cvsInfo 
    let (x,y) = device2pageCoord geometry zmode pcoord 
    connidup   <- connectPenUp   cvsInfo 
    connidmove <- connectPenMove cvsInfo 
    pdraw <-penProcess cid geometry connidmove connidup (empty |> (x,y)) (x,y) 
    (newxoj,bbox) <- liftIO $ addPDraw pinfo currxoj pagenum pdraw
    let bbox' = inflate bbox (get (penWidth.penInfo) xstate) 
        xstate' = set xournalstate (ViewAppendState newxoj) 
                  . updatePageAll (ViewAppendState newxoj)
                  $ xstate
    commit xstate'
    mapM_ (flip invalidateInBBox bbox') . filter (/=cid) $ otherCanvas xstate' 


penProcess :: CanvasId
           -> CanvasPageGeometry
           -> ConnectId DrawingArea -> ConnectId DrawingArea 
           -> Seq (Double,Double) -> (Double,Double) 
           -> Iteratee MyEvent XournalStateIO (Seq (Double,Double))
penProcess cid cpg connidmove connidup pdraw (x0,y0) = do 
  r <- await 
  xstate <- getSt
  let cvsInfo = getCanvasInfo cid xstate
  case r of 
    PenMove _cid' pcoord -> do 
      let canvas = get drawArea cvsInfo
          zmode  = get (zoomMode.viewInfo) cvsInfo
          pcolor = get (penColor.penInfo) xstate 
          pwidth = get (penWidth.penInfo) xstate 
          (x,y) = device2pageCoord cpg zmode pcoord 
          pcolRGBA = fromJust (M.lookup pcolor penColorRGBAmap) 
      liftIO $ drawSegment canvas cpg zmode pwidth pcolRGBA (x0,y0) (x,y)
      penProcess cid cpg connidmove connidup (pdraw |> (x,y)) (x,y) 
    PenUp _cid' pcoord -> do 
      let zmode = get (zoomMode.viewInfo) cvsInfo
          (x,y) = device2pageCoord cpg zmode pcoord 
      disconnect connidmove
      disconnect connidup
      return (pdraw |> (x,y)) 
    _ -> do
      penProcess cid cpg connidmove connidup pdraw (x0,y0) 
