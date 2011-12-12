module Application.HXournal.Iteratee.Select where

import Graphics.UI.Gtk hiding (get,set,disconnect)

import Application.HXournal.Type.Event 
import Application.HXournal.Type.Enum
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

import qualified Data.IntMap as M

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

-- | main mouse pointer click entrance in rectangular selection mode. 
--   choose either starting new rectangular selection or move previously 
--   selected selection. 

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
      let ui = get gtkUIManager xstate
      liftIO $ toggleCutCopyDelete ui (isAnyHitted  selectstrs)
      putSt (set xournalstate (SelectState newtxoj) 
             . updatePageAll (SelectState newtxoj)
             $ xstate) 
      disconnect connidmove
      disconnect connidup 
      invalidateAll 
    _ -> return ()
      
         
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
      moveSelectRectangle cinfo geometry zmode connidmove connidup orig (x,y) 
    PenUp cid' pcoord -> do 
      let (x,y) = device2pageCoord geometry zmode pcoord 
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

deleteSelection :: Iteratee MyEvent XournalStateIO () 
deleteSelection = do 
  liftIO $ putStrLn "delete selection is called"
  xstate <- getSt
  let cinfo = getCurrentCanvasInfo xstate 
      SelectState txoj = get xournalstate xstate 
      Just (n,tpage) = tx_selectpage txoj
  case strokes (tp_firstlayer tpage) of 
    Left _ -> liftIO $ putStrLn "no stroke selection 2 "
    Right alist -> do 
      let newlayer = Left . concat . getA $ alist
          newpage = pageBBoxMapFromTempPageSelect 
                    $ tpage {tp_firstlayer = LayerSelect newlayer} 
          newpages = M.adjust (const newpage) n (tx_pages txoj)
          newtxoj = TempXournalSelect newpages Nothing       
          newxstate = set xournalstate (SelectState newtxoj) xstate
          newxstate' = updatePageAll (SelectState newtxoj) newxstate 
      putSt newxstate' 
      let ui = get gtkUIManager newxstate'
      liftIO $ toggleCutCopyDelete ui False 
      invalidateAll 
          

    
selectPenColorChanged :: PenColor ->  Iteratee MyEvent XournalStateIO () 
selectPenColorChanged pcolor = do 
  liftIO $ putStrLn "selectPenColorChanged called"
  xstate <- getSt
  let cinfo = getCurrentCanvasInfo xstate 
      SelectState txoj = get xournalstate xstate 
      Just (n,tpage) = tx_selectpage txoj
  case strokes (tp_firstlayer tpage) of 
    Left _ -> liftIO $ putStrLn "no stroke selection 2 "
    Right alist -> do 
      let -- newlayer = Left . concat . getA $ alist
          alist' = fmapAL id 
                     (Hitted . map (changeStrokeColor pcolor) . unHitted) alist
          newlayer = Right alist'
          newpage = pageBBoxMapFromTempPageSelect 
                    $ tpage {tp_firstlayer = LayerSelect newlayer} 
          newpages = M.adjust (const newpage) n (tx_pages txoj)
          newtxoj = TempXournalSelect newpages Nothing       
          newxstate = set xournalstate (SelectState newtxoj) xstate
          newxstate' = updatePageAll (SelectState newtxoj) newxstate 
      putSt newxstate' 
      -- let ui = get gtkUIManager newxstate'
      -- liftIO $ toggleCutCopyDelete ui False 
      invalidateAll 
          
selectPenWidthChanged :: Double ->  Iteratee MyEvent XournalStateIO () 
selectPenWidthChanged pwidth = do 
  liftIO $ putStrLn "selectPenWidthChanged called"
  xstate <- getSt
  let cinfo = getCurrentCanvasInfo xstate 
      SelectState txoj = get xournalstate xstate 
      Just (n,tpage) = tx_selectpage txoj
  case strokes (tp_firstlayer tpage) of 
    Left _ -> liftIO $ putStrLn "no stroke selection 2 "
    Right alist -> do 
      let alist' = fmapAL id 
                     (Hitted . map (changeStrokeWidth pwidth) . unHitted) alist
          newlayer = Right alist'
          newpage = pageBBoxMapFromTempPageSelect 
                    $ tpage {tp_firstlayer = LayerSelect newlayer} 
          newpages = M.adjust (const newpage) n (tx_pages txoj)
          newtxoj = TempXournalSelect newpages Nothing       
          newxstate = set xournalstate (SelectState newtxoj) xstate
          newxstate' = updatePageAll (SelectState newtxoj) newxstate 
      putSt newxstate' 
      invalidateAll 

