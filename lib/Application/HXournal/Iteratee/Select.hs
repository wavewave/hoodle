module Application.HXournal.Iteratee.Select where

import Graphics.UI.Gtk hiding (get,set,disconnect)

import Application.HXournal.Type.Event 

import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.XournalState
import Application.HXournal.Accessor
import Application.HXournal.Device
import Application.HXournal.Draw
import Application.HXournal.Iteratee.EventConnect

import Application.HXournal.Iteratee.Draw

import qualified Data.Map as M

import Control.Monad.Trans
import qualified Control.Monad.State as St
import Control.Monad.Coroutine.SuspensionFunctors

import Control.Category
import Data.Label
import Prelude hiding ((.), id)

import Graphics.Xournal.Type
import Graphics.Xournal.HitTest
import Graphics.Xournal.Render.BBox

import Data.Maybe

selectRectStart :: CanvasId -> PointerCoord -> Iteratee MyEvent XournalStateIO () 
selectRectStart cid pcoord = do    
    xstate <- changeCurrentCanvasId cid 
    let cvsInfo = getCanvasInfo cid xstate
        zmode = get (zoomMode.viewInfo) cvsInfo     
    geometry <- getCanvasGeometry cvsInfo
    let (x,y) = device2pageCoord geometry zmode pcoord 
    connidup   <- connectPenUp cvsInfo 
    connidmove <- connectPenMove cvsInfo
    strs <- getAllStrokeBBoxInCurrentPage
    newSelectRectangle cvsInfo geometry zmode connidup connidmove strs (x,y) (x,y)

newSelectRectangle :: CanvasInfo
                   -> CanvasPageGeometry
                   -> ZoomMode
                   -> ConnectId DrawingArea -> ConnectId DrawingArea
                   -> [StrokeBBox] 
                   -> (Double,Double)
                   -> (Double,Double)
                   -> Iteratee MyEvent XournalStateIO ()
newSelectRectangle cinfo geometry zmode connidmove connidup strs orig prev = do  
  let cid = get canvasId cinfo  
  r <- await 
  case r of 
    PenMove cid' pcoord -> do 
      let (x,y) = device2pageCoord geometry zmode pcoord 
      let bbox = BBox orig (x,y)
          prevbbox = BBox orig prev
          hittestbbox = mkHitTestInsideBBox bbox strs
          hittedstrs = concat . map unHitted . getB $ hittestbbox
      invalidateInBBox cid (inflate (fromJust (Just bbox `merge` Just prevbbox)) 2)
      invalidateDrawBBox cid bbox
      mapM_ (invalidateDrawBBox cid . strokebbox_bbox) hittedstrs
      newSelectRectangle cinfo geometry zmode connidmove connidup strs orig (x,y) 
    PenUp cid' pcoord -> do 
      let (x,y) = device2pageCoord geometry zmode pcoord 
      let bbox = BBox orig (x,y)
          prevbbox = BBox orig prev
          hittestbbox = mkHitTestInsideBBox bbox strs
          selectstrs = fmapAL unNotHitted id hittestbbox
          
          hittedstrs = concat . map unHitted . getB $ hittestbbox
      invalidateInBBox cid (fromJust (Just bbox `merge` Just prevbbox))
      mapM_ (invalidateDrawBBox cid . strokebbox_bbox) hittedstrs

    
      disconnect connidmove
      disconnect connidup 
      invalidateAll 
    _ -> return ()
      
selectRectProcess :: CanvasId -> PointerCoord -> Iteratee MyEvent XournalStateIO () 
selectRectProcess cid pcoord = do    
    ev <- await 
    liftIO $ putStrLn "selectRectProcess"
    case ev of 
      PenUp cid' pcoord' -> return ()
      _ -> selectRectProcess cid pcoord 



