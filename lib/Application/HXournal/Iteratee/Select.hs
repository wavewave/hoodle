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

import Application.HXournal.ModelAction.Page
import Application.HXournal.ModelAction.Select

import qualified Data.Map as M

import Control.Monad.Trans
import qualified Control.Monad.State as St
import Control.Monad.Coroutine.SuspensionFunctors

import Control.Category
import Data.Label
import Prelude hiding ((.), id)

import Graphics.Xournal.Type
import Graphics.Xournal.Type.Select
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
    case get currentPage cvsInfo of 
      Right tpage -> if hitInSelection tpage (x,y)
                       then do 
                         -- liftIO $ putStrLn "HITTED"
                         moveSelectRectangle cvsInfo 
                                             geometry 
                                             zmode 
                                             connidup 
                                             connidmove 
                                             (x,y) 
                                             (x,y)
                       else 
                         newSelectRectangle cvsInfo 
                                            geometry 
                                            zmode 
                                            connidup 
                                            connidmove 
                                            strs 
                                            (x,y) 
                                            (x,y)
      Left _ -> newSelectRectangle cvsInfo 
                                   geometry 
                                   zmode 
                                   connidup 
                                   connidmove 
                                   strs 
                                   (x,y) 
                                   (x,y)

      
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
          
      let epage = get currentPage cinfo 
          cpn = get currentPageNum cinfo 
          
      let bbox = BBox orig (x,y)
          prevbbox = BBox orig prev
          hittestbbox = mkHitTestInsideBBox bbox strs
          selectstrs = fmapAL unNotHitted id hittestbbox
          
      xstate <- getSt    
      let SelectState txoj = get xournalstate xstate
          newlayer = LayerSelect (Right selectstrs)
          newpage = case epage of 
                      Left pagebbox -> 
                        let tpg = tempPageSelectFromPageBBoxMap pagebbox
                        in  tpg { tp_firstlayer = newlayer }
                      Right tpage -> tpage { tp_firstlayer = newlayer } 
          newtxoj = txoj { tx_selectpage = Just (cpn,newpage) } 
          
      putSt (set xournalstate (SelectState newtxoj) 
             . updatePageAll (SelectState newtxoj)
             $ xstate)
      disconnect connidmove
      disconnect connidup 
      invalidateAll 
    _ -> return ()
      

hitInSelection :: TempPageSelect -> (Double,Double) -> Bool 
hitInSelection tpage point = 
  let activelayer = tp_firstlayer tpage
  in case strokes activelayer of 
       Left _ -> False   
       Right alist -> 
         let bboxes = map strokebbox_bbox . concatMap unHitted . getB $ alist
         in  any (flip hitTestBBoxPoint point) bboxes 
                      
moveSelectRectangle :: CanvasInfo
                    -> CanvasPageGeometry
                    -> ZoomMode
                    -> ConnectId DrawingArea 
                    -> ConnectId DrawingArea
                    -> (Double,Double)
                    -> (Double,Double)
                    -> Iteratee MyEvent XournalStateIO ()
moveSelectRectangle cinfo geometry zmode connidmove connidup orig@(x0,y0) prev = do
  xstate <- getSt
  let cid = get canvasId cinfo 
  r <- await 
  case r of 
    PenMove cid' pcoord -> do 
      let (x,y) = device2pageCoord geometry zmode pcoord 
      -- liftIO $ putStrLn $ "original = " ++ show orig
      -- liftIO $ putStrLn $ "new = " ++ show (x,y)
      moveSelectRectangle cinfo geometry zmode connidmove connidup orig (x,y) 
    PenUp cid' pcoord -> do 
      let (x,y) = device2pageCoord geometry zmode pcoord 
      -- liftIO $ putStrLn $ "original = " ++ show orig
      -- liftIO $ putStrLn $ "final = " ++ show (x,y)
      
      let offset = (x-x0,y-y0)
          SelectState txoj = get xournalstate xstate
          epage = get currentPage cinfo 
          pagenum = get currentPageNum cinfo
      case epage of 
        Right tpage -> do 
          let newtpage = changeSelectionByOffset tpage offset
              newtxoj = updateTempXournalSelect txoj newtpage pagenum 
          putSt (set xournalstate (SelectState newtxoj)
                 . updatePageAll (SelectState newtxoj) 
                 $ xstate )
        Left _ -> error "this is impossible, in moveSelectRectangle" 
      
      disconnect connidmove
      disconnect connidup 
      invalidateAll 


selectRectProcess :: CanvasId -> PointerCoord -> Iteratee MyEvent XournalStateIO () 
selectRectProcess cid pcoord = do    
    ev <- await 
    liftIO $ putStrLn "selectRectProcess"
    case ev of 
      PenUp cid' pcoord' -> return ()
      _ -> selectRectProcess cid pcoord 


