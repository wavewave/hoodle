
-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.Coroutine.Select 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
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
import Application.HXournal.Util
import Application.HXournal.Coroutine.EventConnect
import Application.HXournal.Coroutine.Draw
import Application.HXournal.Coroutine.Mode
import Application.HXournal.Coroutine.Commit
import Application.HXournal.ModelAction.Page
import Application.HXournal.ModelAction.Select
import Application.HXournal.ModelAction.Layer 
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Compose
import Control.Category
import Control.Applicative 
import Data.Label
import Prelude hiding ((.), id)
import Data.Xournal.Generic
import Data.Xournal.BBox
import Data.Xournal.Select
import Graphics.Xournal.Render.Type
import Graphics.Xournal.Render.BBoxMapPDF
import Graphics.Xournal.Render.HitTest
import Graphics.Xournal.Render.BBox
import Graphics.Rendering.Cairo
import System.IO.Unsafe
import qualified Data.IntMap as IM
import Data.Maybe
import Data.Xournal.Generic
import Graphics.Xournal.Render.Generic
import Graphics.Xournal.Render.PDFBackground
import Graphics.Xournal.Render.BBoxMapPDF
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk hiding (get,set,disconnect)

data TempSelection = TempSelection { tempSurface :: Surface  
                                   , widthHeight :: (Double,Double)
                                   , numSelectedStrokes :: Int } 


uncurry4 :: (a->b->c->d->e)->(a,b,c,d)->e 
uncurry4 f (x,y,z,w) = f x y z w 


predefinedLassoColor :: (Double,Double,Double,Double)
predefinedLassoColor = (1.0,116.0/255.0,0,1)

predefinedLassoWidth :: Double 
predefinedLassoWidth = 4.0

predefinedLassoDash :: ([Double],Double)
predefinedLassoDash = ([10,5],10) 

renderBoxSelection :: BBox -> Render () 
renderBoxSelection bbox = do
  setLineWidth predefinedLassoWidth
  uncurry4 setSourceRGBA predefinedLassoColor
  uncurry setDash predefinedLassoDash 
  let (x1,y1) = bbox_upperleft bbox
      (x2,y2) = bbox_lowerright bbox
  rectangle x1 y1 (x2-x1) (y2-y1)
  stroke

renderSelectedStroke :: StrokeBBox -> Render () 
renderSelectedStroke str = do 
  let bbox = strokebbox_bbox str 
  setLineWidth 0.5
  setSourceRGBA 0 0 1 1
  let (x1,y1) = bbox_upperleft bbox
      (x2,y2) = bbox_lowerright bbox
  rectangle x1 y1 (x2-x1) (y2-y1)
  stroke
   
  
  -- TPageBBoxMapPDFBuf -> (Double,Double) 
                  -- -> Render () -> Surface -> IO ()     
    
updateTempSelection :: TempSelection -> Render () -> IO ()
updateTempSelection tempselection  renderfunc = 
  renderWith (tempSurface tempselection) $ do 
    let (cw,ch) = widthHeight tempselection
    setSourceRGBA 0.5 0.5 0.5 1
    rectangle 0 0 cw ch 
    fill 
    renderfunc    
    

                     

-- | main mouse pointer click entrance in rectangular selection mode. 
--   choose either starting new rectangular selection or move previously 
--   selected selection. 

selectRectStart :: CanvasId -> PointerCoord -> MainCoroutine ()
selectRectStart cid pcoord = do    
    xstate <- changeCurrentCanvasId cid 
    let cvsInfo = getCanvasInfo cid xstate
        zmode = get (zoomMode.viewInfo) cvsInfo     
    geometry <- getCanvasGeometry cvsInfo
    let (x,y) = device2pageCoord geometry zmode pcoord 
    connidup   <- connectPenUp cvsInfo 
    connidmove <- connectPenMove cvsInfo
    strs <- getAllStrokeBBoxInCurrentLayer
    let action (Right tpage) | hitInSelection tpage (x,y) = 
          moveSelectRectangle cvsInfo geometry zmode connidup connidmove (x,y) (x,y)
        action (Right tpage) | otherwise = newSelectAction (gcast tpage :: TPageBBoxMapPDFBuf )
        action (Left page) = newSelectAction page
        newSelectAction page = do   
          let (cw, ch) = (,) <$> floor . fst <*> floor . snd 
                         $ canvas_size geometry 
          let xformfunc = transformForPageCoord geometry zmode
          let renderfunc = do   
                xformfunc 
                cairoRenderOption (InBBoxOption Nothing) (InBBox page) 
                return ()
          tempsurface <- liftIO $ createImageSurface FormatARGB32 cw ch  
          let cwch = (fromIntegral cw, fromIntegral ch)
              tempselection = TempSelection tempsurface cwch 0 
          liftIO $ updateTempSelection tempselection renderfunc 
            -- renderTempPage page cwch renderfunc tempsurface 
          {- liftIO $ renderWith tempsurface $ do 
              setSourceRGBA 0.5 0.5 0.5 1
              rectangle 0 0 (fromIntegral cw) (fromIntegral ch) 
              fill 
              renderfunc -}
          newSelectRectangle cvsInfo  geometry zmode connidup connidmove strs 
                             (x,y) (x,y) tempselection
                             -- (TempSelection tempsurface 0)
          surfaceFinish tempsurface 
    action (get currentPage cvsInfo)      
newSelectRectangle :: CanvasInfo
                   -> CanvasPageGeometry
                   -> ZoomMode
                   -> ConnectId DrawingArea -> ConnectId DrawingArea
                   -> [StrokeBBox] 
                   -> (Double,Double)
                   -> (Double,Double)
                   -> TempSelection -- Surface
                   -> MainCoroutine () 
newSelectRectangle cinfo geometry zmode connidmove connidup strs orig prev 
                   tempselection = do  
  let cid = get canvasId cinfo  
  r <- await 
  case r of 
    PenMove _cid' pcoord -> do 
      let (x,y) = device2pageCoord geometry zmode pcoord 
      let bbox = BBox orig (x,y)
          prevbbox = BBox orig prev
          hittestbbox = mkHitTestInsideBBox bbox strs
          hittedstrs = concat . map unHitted . getB $ hittestbbox
      let newbbox = inflate (fromJust (Just bbox `merge` Just prevbbox)) 5.0
      xstate <- getSt
      let cvsInfo = getCanvasInfo cid xstate 
          page = either id gcast $ get currentPage cvsInfo 
          numselstrs = length hittedstrs 
      when (numselstrs /= numSelectedStrokes tempselection) $ do 
        -- let (cw, ch) = (,) <$> floor . fst <*> floor . snd 
        --                $ canvas_size geometry 
        let xformfunc = transformForPageCoord geometry zmode
        let renderfunc = do   
              xformfunc 
              cairoRenderOption (InBBoxOption Nothing) (InBBox page) 
              mapM_ renderSelectedStroke  hittedstrs
        liftIO $ updateTempSelection tempselection renderfunc
        liftIO $ putStrLn "selection changed!"
      invalidateTemp cid (tempSurface tempselection) 
                         (renderBoxSelection bbox) 
        
        --  >> mapM_ renderSelectedStroke  hittedstrs ) 
        
        -- (render_selection_rect)
      newSelectRectangle cinfo geometry zmode connidmove connidup strs orig 
                         (x,y) 
                         tempselection { numSelectedStrokes = numselstrs }
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
                        let (mcurrlayer,npagebbox) = getCurrentLayerOrSet pagebbox
                            currlayer = maybe (error "newSelectRectangle") id mcurrlayer 
                            newlayer = GLayerBuf (get g_buffer currlayer) (TEitherAlterHitted (Right selectstrs))
                            tpg = gcast npagebbox 
                            ls = get g_layers tpg 
                            npg = tpg { glayers = ls { gselectedlayerbuf = newlayer}  }
                        in npg 
                      Right tpage -> 
                        let ls = glayers tpage 
                            currlayer = gselectedlayerbuf ls
                            newlayer = GLayerBuf (get g_buffer currlayer) (TEitherAlterHitted (Right selectstrs))
                            npage = tpage { glayers = ls { gselectedlayerbuf = newlayer } } 
                        in npage
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
                    -> MainCoroutine ()
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
 
deleteSelection :: MainCoroutine ()
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
          
cutSelection :: MainCoroutine () 
cutSelection = do
  liftIO $ putStrLn "cutSelection called"
  copySelection 
  deleteSelection

copySelection :: MainCoroutine ()
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
              let ui = get gtkUIManager xstate'
              liftIO $ togglePaste ui True 
              putSt xstate'
              invalidateAll 

pasteToSelection :: MainCoroutine () 
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
  
selectPenColorChanged :: PenColor -> MainCoroutine () 
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
          
selectPenWidthChanged :: Double -> MainCoroutine () 
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





