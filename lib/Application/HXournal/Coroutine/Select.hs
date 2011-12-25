module Application.HXournal.Coroutine.Select where

import Graphics.UI.Gtk hiding (get,set,disconnect)
import Application.HXournal.Type.Event 
import Application.HXournal.Type.Enum
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.Clipboard
import Application.HXournal.Type.XournalState
import Application.HXournal.Accessor
import Application.HXournal.Device
import Application.HXournal.Draw
import Application.HXournal.Coroutine.EventConnect
import Application.HXournal.Coroutine.Draw
import Application.HXournal.Coroutine.Mode
import Application.HXournal.Coroutine.Commit
import Application.HXournal.ModelAction.Page
import Application.HXournal.ModelAction.Select
import Control.Monad.Trans
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Category
import Data.Label
import Prelude hiding ((.), id)
import Data.Xournal.Simple
import Data.Xournal.Generic
import Data.Xournal.BBox
import Graphics.Xournal.Render.Type
import Graphics.Xournal.Render.BBoxMapPDF
import Graphics.Xournal.Render.HitTest
import Graphics.Xournal.Render.BBox


import qualified Data.IntMap as IM
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
    PenMove _cid' pcoord -> do 
      let (x,y) = device2pageCoord geometry zmode pcoord 
      let bbox = BBox orig (x,y)
          prevbbox = BBox orig prev
          hittestbbox = mkHitTestInsideBBox bbox strs
          hittedstrs = concat . map unHitted . getB $ hittestbbox
      if not (isBBoxDeltaSmallerThan 1.0 geometry zmode bbox prevbbox) 
         then do flip invalidateWithBufInBBox cid . Just $
                   (inflate (fromJust (Just bbox `merge` Just prevbbox)) 5.0)
                 invalidateDrawBBox cid bbox
                 -- for the time being 
                 -- mapM_ (invalidateDrawBBox cid . strokebbox_bbox) hittedstrs
         else return ()
      newSelectRectangle cinfo geometry zmode connidmove connidup strs orig (x,y) 
    PenUp _cid' pcoord -> do 
      let (x,y) = device2pageCoord geometry zmode pcoord 
      let epage = get currentPage cinfo 
          cpn = get currentPageNum cinfo 
          
      let bbox = BBox orig (x,y)
          hittestbbox = mkHitTestInsideBBox bbox strs
          selectstrs = fmapAL unNotHitted id hittestbbox
      xstate <- getSt    
      let SelectState txoj = get xournalstate xstate
          
          newpage = case epage of 
                      Left pagebbox -> 
                        let currlayer = case IM.lookup 0 (get g_layers pagebbox) of
                              Nothing -> error "something wrong in newSelectRectangle"
                              Just l -> l
                            newlayer = GLayerBuf (get g_buffer currlayer) (TEitherAlterHitted (Right selectstrs))
                         
                        
                            tpg = gcast pagebbox
                            ls = glayers tpg 
                        in  tpg { glayers = ls { gselectedlayerbuf = newlayer}  }
                      Right tpage -> 
                        let ls = glayers tpage 
                            currlayer = gselectedlayerbuf ls
                            newlayer = GLayerBuf (get g_buffer currlayer) (TEitherAlterHitted (Right selectstrs))

                        in  tpage { glayers = ls { gselectedlayerbuf = newlayer } } 
          newtxoj = txoj { gselectSelected = Just (cpn,newpage) } 
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
moveSelectRectangle cinfo geometry zmode connidmove connidup orig@(x0,y0) _prev = do
  xstate <- getSt
  r <- await 
  case r of 
    PenMove _cid' pcoord -> do 
      let (x,y) = device2pageCoord geometry zmode pcoord 
      moveSelectRectangle cinfo geometry zmode connidmove connidup orig (x,y) 
    PenUp _cid' pcoord -> do 
      let (x,y) = device2pageCoord geometry zmode pcoord 
      let offset = (x-x0,y-y0)
          SelectState txoj = get xournalstate xstate
          epage = get currentPage cinfo 
          pagenum = get currentPageNum cinfo
      case epage of 
        Right tpage -> do 
          let newtpage = changeSelectionByOffset tpage offset
          newtxoj <- liftIO $ updateTempXournalSelectIO txoj newtpage pagenum 
          commit . set xournalstate (SelectState newtxoj)
                 . updatePageAll (SelectState newtxoj) 
                 $ xstate 
        Left _ -> error "this is impossible, in moveSelectRectangle" 
      disconnect connidmove
      disconnect connidup 
      invalidateAll 
    _ -> return ()
 
deleteSelection :: Iteratee MyEvent XournalStateIO () 
deleteSelection = do 
  liftIO $ putStrLn "delete selection is called"
  xstate <- getSt
  let SelectState txoj = get xournalstate xstate 
      Just (n,tpage) = gselectSelected txoj
      slayer = gselectedlayerbuf . glayers $ tpage
  case unTEitherAlterHitted . get g_bstrokes $ slayer of 
    Left _ -> liftIO $ putStrLn "no stroke selection 2 "
    Right alist -> do 
      let newlayer = Left . concat . getA $ alist
          oldlayers = glayers tpage
          newpage = tpage { glayers = oldlayers { gselectedlayerbuf = GLayerBuf (get g_buffer slayer) (TEitherAlterHitted newlayer) } } 
      newtxoj <- liftIO $ updateTempXournalSelectIO txoj newpage n          
      let newxstate = updatePageAll (SelectState newtxoj) 
                      . set xournalstate (SelectState newtxoj)
                      $ xstate 
      commit newxstate 
      let ui = get gtkUIManager newxstate
      liftIO $ toggleCutCopyDelete ui False 
      invalidateAll 
          
cutSelection :: Iteratee MyEvent XournalStateIO ()  
cutSelection = do
  liftIO $ putStrLn "cutSelection called"
  copySelection 
  deleteSelection

copySelection :: Iteratee MyEvent XournalStateIO ()
copySelection = do 
  liftIO $ putStrLn "copySelection called"
  xstate <- getSt
  let cinfo = getCurrentCanvasInfo xstate 
      etpage = get currentPage cinfo 
  case etpage of
    Left _ -> return ()
    Right tpage -> do 
      let slayer =  gselectedlayerbuf . glayers $ tpage
      case unTEitherAlterHitted . get g_bstrokes $ slayer of 
        Left _ -> return ()
        Right alist -> do 
          let strs = takeHittedStrokes alist 
          if null strs 
            then return () 
            else do 
              let newclip = Clipboard strs
                  xstate' = set clipboard newclip xstate 
              liftIO $ putStrLn $ "newclipboard with " ++ show strs 
              let ui = get gtkUIManager xstate'
              liftIO $ togglePaste ui True 
              putSt xstate'
              invalidateAll 

pasteToSelection :: Iteratee MyEvent XournalStateIO () 
pasteToSelection = do 
  liftIO $ putStrLn "pasteToSelection called" 
  modeChange ToSelectMode    
  xstate <- getSt
  let SelectState txoj = get xournalstate xstate
      clipstrs = getClipContents . get clipboard $ xstate
      cinfo = getCurrentCanvasInfo xstate 
      pagenum = get currentPageNum cinfo 
      tpage = case get currentPage cinfo of 
                Left pbbox -> (gcast pbbox :: TTempPageSelectPDFBuf)
                Right tp -> tp 
      layerselect = gselectedlayerbuf . glayers $ tpage 
      ls  = glayers tpage
      gbuf = get g_buffer layerselect
      newlayerselect = 
        case unTEitherAlterHitted . get g_bstrokes $ layerselect of 
          Left strs -> (GLayerBuf gbuf . TEitherAlterHitted . Right) (strs :- Hitted clipstrs :- Empty)
          Right alist -> (GLayerBuf gbuf . TEitherAlterHitted . Right) 
                           (concat (interleave id unHitted alist) 
                            :- Hitted clipstrs 
                            :- Empty )
      tpage' = tpage { glayers = ls { gselectedlayerbuf = newlayerselect } } 
  txoj' <- liftIO $ updateTempXournalSelectIO txoj tpage' pagenum 
  let xstate' = updatePageAll (SelectState txoj') 
                . set xournalstate (SelectState txoj') 
                $ xstate 
  commit xstate' 
  let ui = get gtkUIManager xstate' 
  liftIO $ toggleCutCopyDelete ui True
  invalidateAll 
  
selectPenColorChanged :: PenColor ->  Iteratee MyEvent XournalStateIO () 
selectPenColorChanged pcolor = do 
  liftIO $ putStrLn "selectPenColorChanged called"
  xstate <- getSt
  let SelectState txoj = get xournalstate xstate 
      Just (n,tpage) = gselectSelected txoj
      slayer = gselectedlayerbuf . glayers $ tpage
  case unTEitherAlterHitted . get g_bstrokes $ slayer of 
    Left _ -> liftIO $ putStrLn "no stroke selection 2 "
    Right alist -> do 
      let alist' = fmapAL id 
                     (Hitted . map (changeStrokeColor pcolor) . unHitted) alist
          newlayer = Right alist'
          ls = glayers tpage 
          newpage = tpage { glayers = ls { gselectedlayerbuf = GLayerBuf (get g_buffer slayer) (TEitherAlterHitted newlayer) }} 
      newtxoj <- liftIO $ updateTempXournalSelectIO txoj newpage n
      commit . updatePageAll (SelectState newtxoj) 
             . set xournalstate (SelectState newtxoj) 
             $ xstate                       
      invalidateAll 
          
selectPenWidthChanged :: Double ->  Iteratee MyEvent XournalStateIO () 
selectPenWidthChanged pwidth = do 
  liftIO $ putStrLn "selectPenWidthChanged called"
  xstate <- getSt
  let SelectState txoj = get xournalstate xstate 
      Just (n,tpage) = gselectSelected txoj
      slayer = gselectedlayerbuf . get g_layers $ tpage
  case unTEitherAlterHitted . get g_bstrokes $ slayer  of 
    Left _ -> liftIO $ putStrLn "no stroke selection 2 "
    Right alist -> do 
      let alist' = fmapAL id 
                     (Hitted . map (changeStrokeWidth pwidth) . unHitted) alist
          newlayer = Right alist'
          ls = get g_layers tpage 
          newpage = tpage { glayers = ls { gselectedlayerbuf = GLayerBuf (get g_buffer slayer) (TEitherAlterHitted newlayer) }} 
      newtxoj <- liftIO $ updateTempXournalSelectIO txoj newpage n          
      commit . updatePageAll (SelectState newtxoj) 
             . set xournalstate (SelectState newtxoj)
             $ xstate 
      invalidateAll 





