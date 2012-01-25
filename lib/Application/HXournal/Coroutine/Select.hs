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
import Application.HXournal.Coroutine.Pen
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
import Data.Monoid 
import Data.Sequence (Seq(..),(|>))
import qualified Data.Sequence as Sq (empty)
import Data.Time.Clock
import Data.Xournal.Generic
-- import Graphics.Xournal.Render.BBox
import Graphics.Xournal.Render.Simple
import Graphics.Xournal.Render.Generic
import Graphics.Xournal.Render.PDFBackground
import Graphics.Xournal.Render.BBoxMapPDF
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk hiding (get,set,disconnect)

data TempSelectRender a = TempSelectRender { tempSurface :: Surface  
                                           , widthHeight :: (Double,Double)
                                           , tempSelectInfo :: a 
                                           } 

type TempSelection = TempSelectRender [StrokeBBox]

tempSelected :: TempSelection -> [StrokeBBox]
tempSelected = tempSelectInfo 

mkTempSelection :: Surface -> (Double,Double) -> [StrokeBBox] -> TempSelection
mkTempSelection sfc (w,h) strs = TempSelectRender sfc (w,h) strs 

-- | update the content of temp selection. should not be often updated
   
updateTempSelection :: TempSelectRender a -> Render () -> Bool -> IO ()
updateTempSelection tempselection  renderfunc isFullErase = 
  renderWith (tempSurface tempselection) $ do 
    when isFullErase $ do 
      let (cw,ch) = widthHeight tempselection
      setSourceRGBA 0.5 0.5 0.5 1
      rectangle 0 0 cw ch 
      fill 
    renderfunc    
    
dtime_bound :: NominalDiffTime 
dtime_bound = realToFrac (picosecondsToDiffTime 100000000000)

getNewCoordTime :: ((Double,Double),UTCTime) 
                   -> (Double,Double)
                   -> IO (Bool,((Double,Double),UTCTime))
getNewCoordTime (prev,otime) (x,y) = do 
    ntime <- getCurrentTime 
    let dtime = diffUTCTime ntime otime 
        willUpdate = dtime > dtime_bound
        (nprev,nntime) = if dtime > dtime_bound 
                         then ((x,y),ntime)
                         else (prev,otime)
    return (willUpdate,(nprev,nntime))


createTempSelectRender :: CanvasPageGeometry -> ZoomMode 
                          -> TPageBBoxMapPDFBuf
                          -> a 
                          -> MainCoroutine (TempSelectRender a) 
createTempSelectRender geometry zmode page x = do 
    let (cw, ch) = (,) <$> floor . fst <*> floor . snd 
                   $ canvas_size geometry 
        cwch = (fromIntegral cw, fromIntegral ch)
        xformfunc = transformForPageCoord geometry zmode
        renderfunc = do   
          xformfunc 
          cairoRenderOption (InBBoxOption Nothing) (InBBox page) 
          return ()
    tempsurface <- liftIO $ createImageSurface FormatARGB32 cw ch  
    let tempselection = TempSelectRender tempsurface cwch x
    liftIO $ updateTempSelection tempselection renderfunc True
    return tempselection 


-- | main mouse pointer click entrance in rectangular selection mode. 
--   choose either starting new rectangular selection or move previously 
--   selected selection. 

selectRectStart :: CanvasId -> PointerCoord -> MainCoroutine ()
selectRectStart = commonPenStart rectaction 
  where rectaction cinfo cpg zmode (cidup,cidmove) (x,y) = do
          strs <- getAllStrokeBBoxInCurrentLayer
          ctime <- liftIO $ getCurrentTime
          let newSelectAction page = do   
                tsel <- createTempSelectRender cpg zmode page [] 
                newSelectRectangle cinfo cpg zmode cidmove cidup strs 
                                   (x,y) ((x,y),ctime) tsel
                surfaceFinish (tempSurface tsel)          
          let action (Right tpage) | hitInSelection tpage (x,y) = do
                tsel <- createTempSelectRender 
                          cpg zmode (gcast tpage :: TPageBBoxMapPDFBuf)
                          (getSelectedStrokes tpage)
                moveSelect cinfo cpg zmode cidmove cidup 
                           (x,y) ((x,y),ctime) tsel 
                surfaceFinish (tempSurface tsel)
              action (Right tpage) | hitInHandle tpage (x,y) = 
                case getULBBoxFromSelected tpage of 
                  Middle bbox ->  
                    maybe (return ()) (\handle -> do { tsel <- createTempSelectRender cpg zmode (gcast tpage :: TPageBBoxMapPDFBuf) (getSelectedStrokes tpage); resizeSelect handle cinfo cpg zmode cidmove cidup bbox ((x,y),ctime) tsel ; surfaceFinish (tempSurface tsel) }) (checkIfHandleGrasped bbox (x,y))
                  _ -> return () 
              action (Right tpage) | otherwise = newSelectAction (gcast tpage :: TPageBBoxMapPDFBuf )
              action (Left page) = newSelectAction page
          action (get currentPage cinfo)      


newSelectRectangle :: CanvasInfo
                   -> CanvasPageGeometry
                   -> ZoomMode
                   -> ConnectId DrawingArea -> ConnectId DrawingArea
                   -> [StrokeBBox] 
                   -> (Double,Double)
                   -> ((Double,Double),UTCTime)
                   -> TempSelection 
                   -> MainCoroutine () 
newSelectRectangle cinfo geometry zmode connidmove connidup strs orig 
                   (prev,otime) 
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
          (fstrs,sstrs) = separateFS $ getDiffStrokeBBox (tempSelected tempselection) hittedstrs 
      (willUpdate,(ncoord,ntime)) <- liftIO $ getNewCoordTime (prev,otime) (x,y)
      when ((not.null) fstrs || (not.null) sstrs ) $ do 
        let xformfunc = transformForPageCoord geometry zmode
            ulbbox = unUnion . mconcat . fmap (Union .Middle . flip inflate 5 . strokebbox_bbox) $ fstrs
            renderfunc = do   
              xformfunc 
              case ulbbox of 
                Top -> do 
                  cairoRenderOption (InBBoxOption Nothing) (InBBox page) 
                  mapM_ renderSelectedStroke hittedstrs
                Middle bbox -> do 
                  let redrawee = filter (\x -> hitTestBBoxBBox bbox (strokebbox_bbox x) ) hittedstrs  
                  cairoRenderOption (InBBoxOption (Just bbox)) (InBBox page)
                  clipBBox (Just bbox)
                  mapM_ renderSelectedStroke redrawee 
                Bottom -> return ()
              mapM_ renderSelectedStroke sstrs 
        liftIO $ updateTempSelection tempselection renderfunc False
      when willUpdate $  
        invalidateTemp cid (tempSurface tempselection) 
                           (renderBoxSelection bbox) 
      newSelectRectangle cinfo geometry zmode connidmove connidup strs orig 
                         (ncoord,ntime)
                         tempselection { tempSelectInfo = hittedstrs }
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
         
moveSelect :: CanvasInfo
              -> CanvasPageGeometry
              -> ZoomMode
              -> ConnectId DrawingArea 
              -> ConnectId DrawingArea
              -> (Double,Double)
              -> ((Double,Double),UTCTime)
              -> TempSelection
              -> MainCoroutine ()
moveSelect cinfo geometry zmode connidmove connidup orig@(x0,y0) 
           (prev,otime) tempselection = do
  xstate <- getSt
  r <- await 
  case r of 
    PenMove cid pcoord -> do 
      let (x,y) = device2pageCoord geometry zmode pcoord 
      (willUpdate,(ncoord,ntime)) <- liftIO $ getNewCoordTime (prev,otime) (x,y) 
      when willUpdate $ do 
        let strs = tempSelectInfo tempselection
            newstrs = map (changeStrokeBy (offsetFunc (x-x0,y-y0))) strs
            drawselection = do 
              mapM_ (drawOneStroke . gToStroke) newstrs  
        invalidateTemp cid (tempSurface tempselection) drawselection
      moveSelect cinfo geometry zmode connidmove connidup orig (ncoord,ntime) tempselection
    PenUp _cid' pcoord -> do 
      let (x,y) = device2pageCoord geometry zmode pcoord 
      let offset = (x-x0,y-y0)
          SelectState txoj = get xournalstate xstate
          epage = get currentPage cinfo 
          pagenum = get currentPageNum cinfo
      case epage of 
        Right tpage -> do 
          let newtpage = changeSelectionByOffset offset tpage 
          newtxoj <- liftIO $ updateTempXournalSelectIO txoj newtpage pagenum 
          commit . set xournalstate (SelectState newtxoj)
                 . updatePageAll (SelectState newtxoj) 
                 $ xstate 
        Left _ -> error "this is impossible, in moveSelect" 
      disconnect connidmove
      disconnect connidup 
      invalidateAll 
    _ -> return ()
 
resizeSelect :: Handle 
                -> CanvasInfo
                -> CanvasPageGeometry
                -> ZoomMode
                -> ConnectId DrawingArea 
                -> ConnectId DrawingArea
                -> BBox
                -> ((Double,Double),UTCTime)
                -> TempSelection
                -> MainCoroutine ()
resizeSelect handle cinfo geometry zmode connidmove connidup origbbox 
             (prev,otime) tempselection = do
  xstate <- getSt
  r <- await 
  case r of 
    PenMove cid pcoord -> do 
      let (x,y) = device2pageCoord geometry zmode pcoord 
      (willUpdate,(ncoord,ntime)) <- liftIO $ getNewCoordTime (prev,otime) (x,y) 
      when willUpdate $ do 
        let strs = tempSelectInfo tempselection
            sfunc = scaleFromToBBox origbbox newbbox
            newbbox = getNewBBoxFromHandlePos handle origbbox (x,y)            
            newstrs = map (changeStrokeBy sfunc) strs
            drawselection = do 
              mapM_ (drawOneStroke . gToStroke) newstrs  
        invalidateTemp cid (tempSurface tempselection) drawselection
      resizeSelect handle cinfo geometry zmode connidmove connidup 
                   origbbox (ncoord,ntime) tempselection
    PenUp _cid pcoord -> do 
      let (x,y) = device2pageCoord geometry zmode pcoord 
          newbbox = getNewBBoxFromHandlePos handle origbbox (x,y)
          SelectState txoj = get xournalstate xstate
          epage = get currentPage cinfo 
          pagenum = get currentPageNum cinfo
      case epage of 
        Right tpage -> do 
          let sfunc = scaleFromToBBox origbbox newbbox
          let newtpage = changeSelectionBy sfunc tpage 
          newtxoj <- liftIO $ updateTempXournalSelectIO txoj newtpage pagenum 
          commit . set xournalstate (SelectState newtxoj)
                 . updatePageAll (SelectState newtxoj) 
                 $ xstate 
        Left _ -> error "this is impossible, in resizeSelect" 
      disconnect connidmove
      disconnect connidup 
      invalidateAll
      return ()    
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
      case getActiveLayer tpage of 
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
      tpage = either gcast id (get currentPage cinfo)
      layerselect = gselectedlayerbuf . glayers $ tpage 
      ls  = glayers tpage
      gbuf = get g_buffer layerselect
      newlayerselect = case getActiveLayer tpage of 
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

-- | main mouse pointer click entrance in lasso selection mode. 
--   choose either starting new rectangular selection or move previously 
--   selected selection. 

selectLassoStart :: CanvasId -> PointerCoord -> MainCoroutine ()
selectLassoStart = commonPenStart lassoAction 
  where lassoAction cinfo cpg zmode (cidup,cidmove) (x,y) = do 
          strs <- getAllStrokeBBoxInCurrentLayer
          ctime <- liftIO $ getCurrentTime
          let newSelectAction page = do   
                tsel <- createTempSelectRender cpg zmode page [] 
                newSelectLasso cinfo cpg zmode cidmove cidup strs 
                               (x,y) ((x,y),ctime) (Sq.empty |> (x,y)) tsel
                surfaceFinish (tempSurface tsel)          
          let action (Right tpage) | hitInSelection tpage (x,y) = do
                tsel <- createTempSelectRender 
                          cpg zmode (gcast tpage :: TPageBBoxMapPDFBuf)
                          (getSelectedStrokes tpage)
                moveSelect cinfo cpg zmode cidmove cidup 
                           (x,y) ((x,y),ctime) tsel 
                surfaceFinish (tempSurface tsel)
              action (Right tpage) | hitInHandle tpage (x,y) = 
                case getULBBoxFromSelected tpage of 
                  Middle bbox ->  
                    maybe (return ()) (\handle -> do { tsel <- createTempSelectRender cpg zmode (gcast tpage :: TPageBBoxMapPDFBuf) (getSelectedStrokes tpage); resizeSelect handle cinfo cpg zmode cidmove cidup bbox ((x,y),ctime) tsel ; surfaceFinish (tempSurface tsel) }) (checkIfHandleGrasped bbox (x,y))
                  _ -> return () 
              action (Right tpage) | otherwise = newSelectAction (gcast tpage :: TPageBBoxMapPDFBuf )
              action (Left page) = newSelectAction page
          action (get currentPage cinfo)      
          
newSelectLasso :: CanvasInfo
                  -> CanvasPageGeometry
                  -> ZoomMode
                  -> ConnectId DrawingArea -> ConnectId DrawingArea
                  -> [StrokeBBox] 
                  -> (Double,Double)
                  -> ((Double,Double),UTCTime)
                  -> Seq (Double,Double)
                  -> TempSelection 
                  -> MainCoroutine ()
newSelectLasso cinfo cpg zmode cidmove cidup strs orig (prev,otime) lasso tsel = do
  let cid = get canvasId cinfo  
  r <- await 
  case r of 
    PenMove _cid' pcoord -> do 
      let (x,y) = device2pageCoord cpg zmode pcoord 
          nlasso = lasso |> (x,y)
      let lassobbox = mkbboxF nlasso -- BBox orig (x,y)
          -- prevbbox = BBox orig prev
          hittestbbox = mkHitTestInsideBBox lassobbox strs
          hittedstrs = concat . map unHitted . getB $ hittestbbox
      let newbbox = inflate lassobbox 5.0
      xstate <- getSt
      let cvsInfo = getCanvasInfo cid xstate 
          page = either id gcast $ get currentPage cvsInfo 
          numselstrs = length hittedstrs 
          (fstrs,sstrs) = separateFS $ getDiffStrokeBBox (tempSelected tsel) hittedstrs 
      (willUpdate,(ncoord,ntime)) <- liftIO $ getNewCoordTime (prev,otime) (x,y)
      when ((not.null) fstrs || (not.null) sstrs ) $ do 
        let xformfunc = transformForPageCoord cpg zmode
            ulbbox = unUnion . mconcat . fmap (Union .Middle . flip inflate 5 . strokebbox_bbox) $ fstrs
            renderfunc = do   
              xformfunc 
              case ulbbox of 
                Top -> do 
                  cairoRenderOption (InBBoxOption Nothing) (InBBox page) 
                  mapM_ renderSelectedStroke hittedstrs
                Middle bbox -> do 
                  let redrawee = filter (\x -> hitTestBBoxBBox bbox (strokebbox_bbox x) ) hittedstrs  
                  cairoRenderOption (InBBoxOption (Just bbox)) (InBBox page)
                  clipBBox (Just bbox)
                  mapM_ renderSelectedStroke redrawee 
                Bottom -> return ()
              mapM_ renderSelectedStroke sstrs 
        liftIO $ updateTempSelection tsel renderfunc False
      when willUpdate $ do 
        -- let mytest = Sq.empty |> (sqrt 2/2,sqrt 2/2) |> (-sqrt 2/2,sqrt 2/2)
        --                      |> (-sqrt 2/2,-sqrt 2/2) |> (sqrt 2/2,-sqrt 2/2)
        --                      |> (sqrt 2/2,sqrt 2/2)
        -- liftIO $ putStrLn $ "angle = " ++ show (wrappingAngle mytest (0,0))
        liftIO $ putStrLn $ "angle = " ++ show (mappingDegree (nlasso |> orig ) (200,200))
        let test = do { rectangle 195 195 10 10 ; fill } 
        
        invalidateTemp cid (tempSurface tsel) (renderLasso nlasso >> test ) 
      newSelectLasso cinfo cpg zmode cidmove cidup strs orig 
                     (ncoord,ntime) nlasso 
                     tsel { tempSelectInfo = hittedstrs }
    PenUp _cid' pcoord -> do 
      let (x,y) = device2pageCoord cpg zmode pcoord 
          nlasso = lasso |> (x,y)
      let epage = get currentPage cinfo 
          cpn = get currentPageNum cinfo 
      let bbox = mkbboxF nlasso -- BBox orig (x,y)
          hittestbbox = mkHitTestInsideBBox bbox strs
          selectstrs = fmapAL unNotHitted id hittestbbox
      xstate <- getSt    
      let SelectState txoj = get xournalstate xstate
          newpage = case epage of 
                      Left pagebbox -> 
                        let (mcurrlayer,npagebbox) = getCurrentLayerOrSet pagebbox
                            currlayer = maybe (error "newSelectLasso") id mcurrlayer 
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
      disconnect cidmove
      disconnect cidup 
      invalidateAll 
    _ -> return ()
  
  
  
  -- liftIO $ putStrLn "newSelectLasso" 
  


       







