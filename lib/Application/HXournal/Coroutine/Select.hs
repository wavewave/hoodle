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
-----------------------------------------------------------------------------

module Application.HXournal.Coroutine.Select where

import Graphics.UI.Gtk hiding (get,set,disconnect)
import Application.HXournal.Type.Event 
import Application.HXournal.Type.Enum
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.Clipboard
import Application.HXournal.Type.PageArrangement
import Application.HXournal.Type.XournalState
import Application.HXournal.Type.Alias
import Application.HXournal.Accessor
import Application.HXournal.Device
import Application.HXournal.View.Draw
import Application.HXournal.View.Coordinate
import Application.HXournal.Coroutine.EventConnect
import Application.HXournal.Coroutine.Draw
import Application.HXournal.Coroutine.Pen
import Application.HXournal.Coroutine.Mode
import Application.HXournal.Coroutine.Commit
import Application.HXournal.ModelAction.Page
import Application.HXournal.ModelAction.Select
import Application.HXournal.ModelAction.Layer 
import Application.HXournal.Util
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Category
import Control.Applicative 
import Data.Label
import Prelude hiding ((.), id)
import Data.Xournal.Simple (Dimension(..))
import Data.Xournal.Generic
import Data.Xournal.BBox
import Graphics.Rendering.Cairo
import Data.Monoid 
import qualified Data.IntMap as M
import Data.Sequence (Seq,(|>))
import qualified Data.Sequence as Sq (empty)
import Data.Time.Clock
import Graphics.Xournal.Render.Type
import Graphics.Xournal.Render.BBoxMapPDF
import Graphics.Xournal.Render.HitTest
import Graphics.Xournal.Render.BBox
import Graphics.Xournal.Render.Simple
import Graphics.Xournal.Render.Generic

-- |

createTempSelectRender :: PageNum -> CanvasGeometry -> Page EditMode
                          -> a 
                          -> MainCoroutine (TempSelectRender a) 
createTempSelectRender pnum geometry page x = do 
    let Dim cw ch = unCanvasDimension . canvasDim $ geometry
        xformfunc = cairoXform4PageCoordinate geometry pnum 
        renderfunc = do   
          xformfunc 
          cairoRenderOption (InBBoxOption Nothing) (InBBox page) 
          return ()
    tempsurface <- liftIO $ createImageSurface FormatARGB32 (floor cw) (floor ch)  
    let tempselection = TempSelectRender tempsurface (cw,ch) x
    liftIO $ updateTempSelection tempselection renderfunc True
    return tempselection 


-- | main mouse pointer click entrance in rectangular selection mode. 
--   choose either starting new rectangular selection or move previously 
--   selected selection. 

selectRectStart :: CanvasId -> PointerCoord -> MainCoroutine ()
selectRectStart cid = commonPenStart rectaction cid
  where rectaction cinfo pnum geometry (cidup,cidmove) (x,y) = do
          strs <- getAllStrokeBBoxInCurrentLayer
          ctime <- liftIO $ getCurrentTime
          let newSelectAction page = do   
                tsel <- createTempSelectRender pnum geometry page [] 
                newSelectRectangle cid pnum geometry cidmove cidup strs 
                                   (x,y) ((x,y),ctime) tsel
                surfaceFinish (tempSurface tsel)          
          let 
              action (Right tpage) | hitInHandle tpage (x,y) = 
                case getULBBoxFromSelected tpage of 
                  Middle bbox ->  
                    maybe (return ()) (\handle -> do { tsel <- createTempSelectRender pnum geometry (gcast tpage :: Page EditMode) (getSelectedStrokes tpage); resizeSelect handle cid pnum geometry cidmove cidup bbox ((x,y),ctime) tsel ; surfaceFinish (tempSurface tsel) }) (checkIfHandleGrasped bbox (x,y))
                  _ -> return () 
              action (Right tpage) | hitInSelection tpage (x,y) = do
                tsel <- createTempSelectRender pnum geometry
                          (gcast tpage :: Page EditMode) (getSelectedStrokes tpage)
                moveSelect cid pnum geometry cidmove cidup 
                           (x,y) ((x,y),ctime) tsel 
                surfaceFinish (tempSurface tsel)                  
                  
              action (Right tpage) | otherwise = newSelectAction (gcast tpage :: Page EditMode )
              action (Left page) = newSelectAction page
          action (get currentPage cinfo)      


newSelectRectangle :: CanvasId
                   -> PageNum 
                   -> CanvasGeometry
                   -> ConnectId DrawingArea -> ConnectId DrawingArea
                   -> [StrokeBBox] 
                   -> (Double,Double)
                   -> ((Double,Double),UTCTime)
                   -> TempSelection 
                   -> MainCoroutine () 
newSelectRectangle cid pnum geometry connidmove connidup strs orig 
                   (prev,otime) tempselection = do  
    r <- await 
    xst <- getSt 
    selectBoxAction (fsingle r xst) (fsingle r xst) . getCanvasInfo cid $ xst
  where 
    fsingle r xstate cinfo = penMoveAndUpOnly r pnum geometry defact
                               (moveact xstate cinfo) (upact xstate cinfo)
    defact = newSelectRectangle cid pnum geometry connidmove connidup strs orig 
                         (prev,otime) tempselection 
    moveact xstate cinfo (x,y) = do 
      let bbox = BBox orig (x,y)
          hittestbbox = mkHitTestInsideBBox bbox strs
          hittedstrs = concat . map unHitted . getB $ hittestbbox
      let page = either id gcast $ get currentPage cinfo 
          (fstrs,sstrs) = separateFS $ getDiffStrokeBBox (tempSelected tempselection) hittedstrs 
      (willUpdate,(ncoord,ntime)) <- liftIO $ getNewCoordTime (prev,otime) (x,y)
      when ((not.null) fstrs || (not.null) sstrs ) $ do 
        let xformfunc = cairoXform4PageCoordinate geometry pnum 
            ulbbox = unUnion . mconcat . fmap (Union .Middle . flip inflate 5 . strokebbox_bbox) $ fstrs
            renderfunc = do   
              xformfunc 
              case ulbbox of 
                Top -> do 
                  cairoRenderOption (InBBoxOption Nothing) (InBBox page) 
                  mapM_ renderSelectedStroke hittedstrs
                Middle sbbox -> do 
                  let redrawee = filter (hitTestBBoxBBox sbbox.strokebbox_bbox) hittedstrs  
                  cairoRenderOption (InBBoxOption (Just sbbox)) (InBBox page)
                  clipBBox (Just sbbox)
                  mapM_ renderSelectedStroke redrawee 
                Bottom -> return ()
              mapM_ renderSelectedStroke sstrs 
        liftIO $ updateTempSelection tempselection renderfunc False
      when willUpdate $  
        invalidateTemp cid (tempSurface tempselection) 
                           (renderBoxSelection bbox) 
      newSelectRectangle cid pnum geometry connidmove connidup strs orig 
                         (ncoord,ntime)
                         tempselection { tempSelectInfo = hittedstrs }
    upact xstate cinfo pcoord = do       
      let pagecoord = desktop2Page geometry . device2Desktop geometry $ pcoord 
          (x,y) = runIdentity $ skipIfNotInSamePage pnum geometry pcoord (return prev) return
      let epage = get currentPage cinfo 
          cpn = get currentPageNum cinfo 
      let bbox = BBox orig (x,y)
          hittestbbox = mkHitTestInsideBBox bbox strs
          selectstrs = fmapAL unNotHitted id hittestbbox
      xstate <- getSt    
      let SelectState txoj = get xournalstate xstate
          newpage = case epage of 
                      Left pagebbox -> makePageSelectMode pagebbox selectstrs 
                      Right tpage -> 
                        let ls = glayers tpage 
                            currlayer = gselectedlayerbuf ls
                            newlayer = GLayerBuf (get g_buffer currlayer) (TEitherAlterHitted (Right selectstrs))
                            npage = tpage { glayers = ls { gselectedlayerbuf = newlayer } } 
                        in npage
          newtxoj = txoj { gselectSelected = Just (cpn,newpage) } 
      let ui = get gtkUIManager xstate
      liftIO $ toggleCutCopyDelete ui (isAnyHitted  selectstrs)
      putSt . set xournalstate (SelectState newtxoj) 
            =<< (liftIO (updatePageAll (SelectState newtxoj) xstate))
      disconnect connidmove
      disconnect connidup 
      invalidateAll 

{-                        let (mcurrlayer,npagebbox) = getCurrentLayerOrSet pagebbox
                            currlayer = maybe (error "newSelectRectangle") id mcurrlayer 
                            newlayer = GLayerBuf (get g_buffer currlayer) (TEitherAlterHitted (Right selectstrs))
                            tpg = gcast npagebbox 
                            ls = get g_layers tpg 
                            npg = tpg { glayers = ls { gselectedlayerbuf = newlayer}  }
                        in npg -}







-- | 
         
moveSelect :: CanvasId
              -> PageNum -- ^ starting pagenum 
              -> CanvasGeometry
              -> ConnectId DrawingArea 
              -> ConnectId DrawingArea
              -> (Double,Double)
              -> ((Double,Double),UTCTime)
              -> TempSelection
              -> MainCoroutine ()
moveSelect cid pnum geometry connidmove connidup orig@(x0,y0) 
           (prev,otime) tempselection = do
    xst <- getSt
    r <- await 
    selectBoxAction (fsingle r xst) (fsingle r xst) . getCanvasInfo cid $ xst 
  where 
    fsingle r xstate cinfo = 
      penMoveAndUpInterPage r pnum geometry defact (moveact xstate cinfo) (upact xstate cinfo) 
    defact = moveSelect cid pnum geometry connidmove connidup orig (prev,otime) 
               tempselection
    moveact xstate cinfo oldpgn pcpair@(newpgn,pgxy@(PageCoord (px,py))) = do 
      let (x,y) 
            | oldpgn == newpgn = (px,py) 
            | otherwise = 
              let DeskCoord (xo,yo) = page2Desktop geometry (oldpgn,PageCoord (0,0))
                  DeskCoord (xn,yn) = page2Desktop geometry pcpair 
              in (xn-xo,yn-yo)
      
      (willUpdate,(ncoord,ntime)) <- liftIO $ getNewCoordTime (prev,otime) (x,y) 
      when willUpdate $ do 
        let strs = tempSelectInfo tempselection
            newstrs = map (changeStrokeBy (offsetFunc (x-x0,y-y0))) strs
            drawselection = do 
              mapM_ (drawOneStroke . gToStroke) newstrs  
        invalidateTempBasePage cid (tempSurface tempselection) pnum drawselection
      moveSelect cid pnum geometry connidmove connidup orig (ncoord,ntime) 
        tempselection
    upact :: HXournalState -> CanvasInfo a -> PointerCoord -> MainCoroutine () 
    upact xst cinfo pcoord = 
      switchActionEnteringDiffPage pnum geometry pcoord (return ()) 
        (chgaction xst cinfo) 
        (ordaction xst cinfo)
            
    chgaction xstate cinfo oldpgn (newpgn,PageCoord (x,y)) = do 
      let SelectState txoj = get xournalstate xstate
          epage = get currentPage cinfo 
      (xstate1,ntxoj1) <- case epage of 
        Right oldtpage -> do 
          let oldtpage' = deleteSelected oldtpage
          ntxoj <- liftIO $ updateTempXournalSelectIO txoj oldtpage' (unPageNum oldpgn)
          xst <- return . set xournalstate (SelectState ntxoj)
                   =<< (liftIO (updatePageAll (SelectState ntxoj) xstate)) 
          return (xst,ntxoj)       
        Left _ -> error "this is impossible, in moveSelect" 
      let mpg = M.lookup (unPageNum newpgn) (get g_selectAll ntxoj1)
      maybeFlip mpg (commit xstate1) $ \page -> do   
        let xstate2 = xstate1 
        commit xstate2
      disconnect connidmove
      disconnect connidup 
      invalidateAll 
      
      
      
    ordaction xstate cinfo _pgn (_cpn,PageCoord (x,y)) = do 
      let offset = (x-x0,y-y0)
          SelectState txoj = get xournalstate xstate
          epage = get currentPage cinfo 
          pagenum = get currentPageNum cinfo
      case epage of 
        Right tpage -> do 
          let newtpage = changeSelectionByOffset offset tpage 
          newtxoj <- liftIO $ updateTempXournalSelectIO txoj newtpage pagenum 
          commit . set xournalstate (SelectState newtxoj)
                 =<< (liftIO (updatePageAll (SelectState newtxoj) xstate))
        Left _ -> error "this is impossible, in moveSelect" 
      disconnect connidmove
      disconnect connidup 
      invalidateAll 
      
      
      -- return ()                             
       --  runIdentity $ skipIfNotInSamePage pnum geometry pcoord (return prev) return
      -- penMoveAndUpOnly r pnum geometry defact (moveact xstate cinfo) (upact xstate cinfo) 

-- |
      
resizeSelect :: Handle 
                -> CanvasId
                -> PageNum 
                -> CanvasGeometry
                -> ConnectId DrawingArea 
                -> ConnectId DrawingArea
                -> BBox
                -> ((Double,Double),UTCTime)
                -> TempSelection
                -> MainCoroutine ()
resizeSelect handle cid pnum geometry connidmove connidup origbbox 
             (prev,otime) tempselection = do
    xst <- getSt
    r <- await 
    selectBoxAction (fsingle r xst) (fsingle r xst) . getCanvasInfo cid $ xst
  where
    fsingle r xstate cinfo = penMoveAndUpOnly r pnum geometry defact (moveact xstate cinfo) (upact xstate cinfo)
    defact = resizeSelect handle cid pnum geometry connidmove connidup 
               origbbox (prev,otime) tempselection
    moveact xstate cinfo (x,y) = do 
      (willUpdate,(ncoord,ntime)) <- liftIO $ getNewCoordTime (prev,otime) (x,y) 
      when willUpdate $ do 
        let strs = tempSelectInfo tempselection
            sfunc = scaleFromToBBox origbbox newbbox
            newbbox = getNewBBoxFromHandlePos handle origbbox (x,y)            
            newstrs = map (changeStrokeBy sfunc) strs
            drawselection = do 
              mapM_ (drawOneStroke . gToStroke) newstrs  
        invalidateTemp cid (tempSurface tempselection) drawselection
      resizeSelect handle cid pnum geometry connidmove connidup 
                   origbbox (ncoord,ntime) tempselection
    upact xstate cinfo pcoord = do 
      let pagecoord = desktop2Page geometry . device2Desktop geometry $ pcoord 
          (x,y) = runIdentity $ skipIfNotInSamePage pnum geometry pcoord (return prev) return
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
                 =<< (liftIO (updatePageAll (SelectState newtxoj) xstate))
        Left _ -> error "this is impossible, in resizeSelect" 
      disconnect connidmove
      disconnect connidup 
      invalidateAll
      return ()    

-- |

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
      newxstate <- liftIO $ updatePageAll (SelectState newtxoj) 
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
copySelection = updateXState copySelectionAction >> invalidateAll 
  where copySelectionAction xst = 
          selectBoxAction (fsingle xst) (fsingle xst) . get currentCanvasInfo $ xst
        fsingle xstate cinfo = maybe (return xstate) id $ 
          eitherMaybe (get currentPage cinfo) `pipe` getActiveLayer 
                                              `pipe` (Right . xstateadj . takeHittedStrokes)
          where eitherMaybe (Left _) = Nothing
                eitherMaybe (Right a) = Just a 
                x `pipe` a = x >>= eitherMaybe . a 
                infixl 6 `pipe`
                xstateadj strs | null strs = return xstate
                               | otherwise = do let newclip = Clipboard strs
                                                    xstate' = set clipboard newclip xstate 
                                                    ui = get gtkUIManager xstate'
                                                liftIO $ togglePaste ui True 
                                                return xstate' 

pasteToSelection :: MainCoroutine () 
pasteToSelection = modeChange ToSelectMode >> updateXState pasteAction >> invalidateAll  
  where pasteAction xst = 
          boxAction (fsimple xst) . get currentCanvasInfo $ xst
        fsimple xstate cinfo = do 
          let SelectState txoj = get xournalstate xstate
              clipstrs = getClipContents . get clipboard $ xstate
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
          xstate' <- liftIO $ updatePageAll (SelectState txoj') 
                              . set xournalstate (SelectState txoj') 
                              $ xstate 
          commit xstate' 
          let ui = get gtkUIManager xstate' 
          liftIO $ toggleCutCopyDelete ui True
          return xstate' 
        
-- | 
  
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
      commit =<< liftIO (updatePageAll (SelectState newtxoj)
                        . set xournalstate (SelectState newtxoj) $ xstate )
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
      commit =<< liftIO (updatePageAll (SelectState newtxoj) 
                         . set xournalstate (SelectState newtxoj) $ xstate )
      invalidateAll 

-- | main mouse pointer click entrance in lasso selection mode. 
--   choose either starting new rectangular selection or move previously 
--   selected selection. 

selectLassoStart :: CanvasId -> PointerCoord -> MainCoroutine ()
selectLassoStart cid = commonPenStart lassoAction cid 
  where lassoAction cinfo pnum geometry (cidup,cidmove) (x,y) = do 
          strs <- getAllStrokeBBoxInCurrentLayer
          ctime <- liftIO $ getCurrentTime
          let newSelectAction page = do   
                tsel <- createTempSelectRender pnum geometry page [] 
                newSelectLasso cinfo pnum geometry cidmove cidup strs 
                               (x,y) ((x,y),ctime) (Sq.empty |> (x,y)) tsel
                surfaceFinish (tempSurface tsel)          
          let action (Right tpage) | hitInSelection tpage (x,y) = do
                tsel <- createTempSelectRender 
                          pnum geometry (gcast tpage :: Page EditMode)
                          (getSelectedStrokes tpage)
                moveSelect (get canvasId cinfo) pnum geometry cidmove cidup 
                           (x,y) ((x,y),ctime) tsel 
                surfaceFinish (tempSurface tsel)
              action (Right tpage) | hitInHandle tpage (x,y) = 
                case getULBBoxFromSelected tpage of 
                  Middle bbox ->  
                    maybe (return ()) (\handle -> do { tsel <- createTempSelectRender pnum geometry (gcast tpage :: Page EditMode) (getSelectedStrokes tpage); resizeSelect handle cid pnum geometry cidmove cidup bbox ((x,y),ctime) tsel ; surfaceFinish (tempSurface tsel) }) (checkIfHandleGrasped bbox (x,y))
                  _ -> return () 
              action (Right tpage) | otherwise = newSelectAction (gcast tpage :: Page EditMode )
              action (Left page) = newSelectAction page
          action (get currentPage cinfo)      
          
newSelectLasso :: (ViewMode a) => CanvasInfo a
                  -> PageNum 
                  -> CanvasGeometry
                  -> ConnectId DrawingArea -> ConnectId DrawingArea
                  -> [StrokeBBox] 
                  -> (Double,Double)
                  -> ((Double,Double),UTCTime)
                  -> Seq (Double,Double)
                  -> TempSelection 
                  -> MainCoroutine ()
newSelectLasso cvsInfo pnum geometry cidmove cidup strs orig (prev,otime) lasso tsel = do
    r <- await 
    fsingle r cvsInfo 
  where  
    fsingle r cinfo = penMoveAndUpOnly r pnum geometry defact
                        (moveact cinfo) (upact cinfo)
    defact = newSelectLasso cvsInfo pnum geometry cidmove cidup strs orig 
               (prev,otime) lasso tsel
    moveact cinfo (x,y) = do 
      let nlasso = lasso |> (x,y)
      (willUpdate,(ncoord,ntime)) <- liftIO $ getNewCoordTime (prev,otime) (x,y)
      when willUpdate $ do 
        invalidateTemp (get canvasId cinfo) (tempSurface tsel) (renderLasso nlasso) 
      newSelectLasso cinfo pnum geometry cidmove cidup strs orig 
                     (ncoord,ntime) nlasso tsel
    upact cinfo pcoord = do 
      let pagecoord = desktop2Page geometry . device2Desktop geometry $ pcoord 
          (x,y) = runIdentity $ skipIfNotInSamePage pnum geometry pcoord (return prev) return
          nlasso = lasso |> (x,y)
      let epage = get currentPage cinfo 
          cpn = get currentPageNum cinfo 
      let hittestlasso = mkHitTestAL (hitLassoStroke (nlasso |> orig)) strs
          selectstrs = fmapAL unNotHitted id hittestlasso
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
      putSt . set xournalstate (SelectState newtxoj) 
            =<< (liftIO (updatePageAll (SelectState newtxoj) xstate))
      disconnect cidmove
      disconnect cidup 
      invalidateAll 






