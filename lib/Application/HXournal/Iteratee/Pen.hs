module Application.HXournal.Iteratee.Pen where

import Graphics.UI.Gtk hiding (get,set)

import Application.HXournal.Device 
import Application.HXournal.Type.Event
import Application.HXournal.Type.Enum
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.XournalState
import Application.HXournal.Type.Event

import Application.HXournal.Iteratee.Draw
import Application.HXournal.Iteratee.EventConnect
import Application.HXournal.ModelAction.Pen

import Application.HXournal.Draw

import Control.Monad.Trans
import qualified Control.Monad.State as St
import Control.Monad.Coroutine.SuspensionFunctors

import Data.Sequence
import qualified Data.Map as M
import Data.Maybe 

import Control.Category
import Data.Label
import Prelude hiding ((.), id)

import Text.Xournal.Type 

penStart :: PointerCoord -> Iteratee MyEvent XournalStateIO ()
penStart pcoord = do 
    xstate <- lift St.get 
    let currCvsId = get currentCanvas xstate 
        maybeCurrCvs = M.lookup currCvsId (get canvasInfoMap xstate)
    case maybeCurrCvs of 
      Nothing -> return ()
      Just currCvsInfo -> do 
        let currxoj = get xournalbbox xstate        
            canvas = get drawArea currCvsInfo   
            pagenum = get currentPageNum currCvsInfo
            page = (!!pagenum) . xournalPages $ currxoj
            (x0,y0) = get (viewPortOrigin.viewInfo) currCvsInfo
            pinfo = get penInfo xstate
            zmode = get (zoomMode.viewInfo) currCvsInfo
        geometry <- liftIO (getCanvasPageGeometry canvas page (x0,y0) )
        let (x,y) = device2pageCoord geometry zmode pcoord 
        connidup <- connPenUp canvas      
        connidmove <- connPenMove canvas
        pdraw <- penProcess geometry connidmove connidup (empty |> (x,y)) (x,y) 
        let newxoj = addPDraw pinfo currxoj pagenum pdraw
        lift . St.put . set xournalbbox newxoj $ xstate 
        return ()

penProcess :: CanvasPageGeometry
           -> ConnectId DrawingArea -> ConnectId DrawingArea 
           -> Seq (Double,Double) -> (Double,Double) 
           -> Iteratee MyEvent XournalStateIO (Seq (Double,Double))
penProcess cpg connidmove connidup pdraw (x0,y0) = do 
  r <- await 
  xstate <- lift St.get
  let currCvsId = get currentCanvas xstate 
      maybeCurrCvs = M.lookup currCvsId (get canvasInfoMap xstate)
  case maybeCurrCvs of 
    Nothing -> error "something wrong" 
    Just currCvsInfo -> do 
      case r of 
        PenMove pcoord -> do 
          let canvas = get drawArea currCvsInfo
              zmode  = get (zoomMode.viewInfo) currCvsInfo
              pcolor = get (penColor.penInfo) xstate 
              pwidth = get (penWidth.penInfo) xstate 
              (x,y) = device2pageCoord cpg zmode pcoord 
              pcolRGBA = fromJust (M.lookup pcolor penColorRGBAmap) 
          liftIO $ drawSegment canvas cpg zmode pwidth pcolRGBA (x0,y0) (x,y)
          penProcess cpg connidmove connidup (pdraw |> (x,y)) (x,y) 
        PenUp pcoord -> do 
          let zmode = get (zoomMode.viewInfo) currCvsInfo
              (x,y) = device2pageCoord cpg zmode pcoord 
          liftIO $ signalDisconnect connidmove
          liftIO $ signalDisconnect connidup
          return (pdraw |> (x,y)) 
        other -> do
          -- defaultEventProcess other        
          penProcess cpg connidmove connidup pdraw (x0,y0) 
