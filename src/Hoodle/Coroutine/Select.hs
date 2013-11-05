-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Select 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- selection-related coroutines  
-- 
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Select where

-- from other package 
import           Control.Category
import           Control.Lens (view,set)
import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.State
import           Data.Monoid 
import qualified Data.IntMap as M
import           Data.Sequence (Seq,(|>))
import qualified Data.Sequence as Sq (empty)
import           Data.Time.Clock
import           Graphics.Rendering.Cairo
import qualified Graphics.Rendering.Cairo.Matrix as Mat
-- from hoodle-platform
import           Data.Hoodle.Select
import           Data.Hoodle.Simple (Dimension(..))
import           Data.Hoodle.Generic
import           Data.Hoodle.BBox
import           Graphics.Hoodle.Render.Generic
import           Graphics.Hoodle.Render.Util
import           Graphics.Hoodle.Render.Util.HitTest
import           Graphics.Hoodle.Render.Type
import           Graphics.Hoodle.Render.Type.HitTest
-- from this package
import           Hoodle.Accessor
import           Hoodle.Device
import           Hoodle.Coroutine.Commit
import           Hoodle.Coroutine.ContextMenu 
import           Hoodle.Coroutine.Draw
import           Hoodle.Coroutine.Mode
import           Hoodle.Coroutine.Pen
import           Hoodle.ModelAction.Layer 
import           Hoodle.ModelAction.Page
import           Hoodle.ModelAction.Pen
import           Hoodle.ModelAction.Select
import           Hoodle.ModelAction.Select.Transform
import           Hoodle.Type.Alias
import           Hoodle.Type.Canvas
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Enum
import           Hoodle.Type.Event 
import           Hoodle.Type.PageArrangement
import           Hoodle.Type.HoodleState
import           Hoodle.View.Coordinate
import           Hoodle.View.Draw
-- 
import           Prelude hiding ((.), id)


{-
-- |
createTempSelectRender :: PageNum -> CanvasGeometry -> Page EditMode
                          -> a 
                          -> MainCoroutine (TempSelectRender a) 
createTempSelectRender _pnum geometry _page x = do 
    xst <- get
    let hdl = getHoodle xst
    let Dim cw ch = unCanvasDimension . canvasDim $ geometry
    (tempsurface,_) <- liftIO $ canvasImageSurface Nothing geometry hdl 
    let tempselection = TempSelectRender tempsurface (cw,ch) x
    return tempselection 
-}

-- | For Selection mode from pen mode with 2nd pen button
dealWithOneTimeSelectMode :: MainCoroutine ()     -- ^ main action 
                             -> MainCoroutine ()  -- ^ terminating action
                             -> MainCoroutine ()
dealWithOneTimeSelectMode action terminator = do 
  xstate <- get 
  case view isOneTimeSelectMode xstate of 
    NoOneTimeSelectMode -> action 
    YesBeforeSelect -> 
      action >> updateXState (return . set isOneTimeSelectMode YesAfterSelect)
    YesAfterSelect -> do 
      terminator 
      updateXState (return . set isOneTimeSelectMode NoOneTimeSelectMode) 
      modeChange ToViewAppendMode

-- | main mouse pointer click entrance in rectangular selection mode. 
--   choose either starting new rectangular selection or move previously 
--   selected selection. 
--   (dev note: need to be refactored with selectLassoStart)
selectRectStart :: PenButton -> CanvasId -> PointerCoord -> MainCoroutine ()
selectRectStart pbtn cid = commonPenStart rectaction cid
  where rectaction cinfo pnum geometry (x,y) = do
          itms <- rItmsInCurrLyr
          ctime <- liftIO $ getCurrentTime
          let newSelectAction page = 
                dealWithOneTimeSelectMode 
                  (do tsel <- createTempRender pnum geometry page [] 
                      newSelectRectangle cid pnum geometry itms 
                                         (x,y) ((x,y),ctime) tsel
                      surfaceFinish (tempSurfaceSrc tsel) 
                      showContextMenu (pnum,(x,y))
                  )
                  (return ())  
              action (Right tpage) | hitInHandle tpage (x,y) = 
                case getULBBoxFromSelected tpage of 
                  Middle bbox ->  
                    maybe (return ()) 
                          (\handle -> startResizeSelect 
                                        handle cid pnum geometry 
                                        bbox ((x,y),ctime) tpage)
                          (checkIfHandleGrasped bbox (x,y))
                  _ -> return () 
              action (Right tpage) | hitInSelection tpage (x,y) = 
                case pbtn of
                  PenButton1 -> startMoveSelect cid pnum geometry ((x,y),ctime) tpage
                  PenButton3 -> do 
                    waitSomeEvent (\e -> case e of PenUp _ _ -> True ; _ -> False) 
                    showContextMenu (pnum,(x,y))                    
                  _ -> return () 
              action (Right tpage) | otherwise = newSelectAction (hPage2RPage tpage)
              action (Left page) = newSelectAction page
          xstate <- get 
          let hdlmodst = view hoodleModeState xstate 
          let epage = getCurrentPageEitherFromHoodleModeState cinfo hdlmodst 
          action epage

-- | 
newSelectRectangle :: CanvasId
                   -> PageNum 
                   -> CanvasGeometry
                   -> [RItem] 
                   -> (Double,Double)
                   -> ((Double,Double),UTCTime)
                   -> TempSelection 
                   -> MainCoroutine () 
newSelectRectangle cid pnum geometry itms orig 
                   (prev,otime) tempselection = do  
    r <- nextevent
    xst <- get 
    unboxAct (fsingle r xst) . getCanvasInfo cid $ xst
  where 
    fsingle r xstate cinfo = penMoveAndUpOnly r pnum geometry defact
                               (moveact xstate cinfo) (upact xstate cinfo)
    defact = newSelectRectangle cid pnum geometry itms orig 
                         (prev,otime) tempselection 
    moveact _xstate _cinfo (_pcoord,(x,y)) = do 
      let bbox = BBox orig (x,y)
          hittestbbox = hltEmbeddedByBBox bbox itms
          hitteditms = takeHitted hittestbbox
      page <- getCurrentPageCvsId cid
      let (fitms,sitms) = separateFS $ getDiffBBox (tempInfo tempselection) hitteditms 
      (willUpdate,(ncoord,ntime)) <- liftIO $ getNewCoordTime (prev,otime) (x,y)
      when ((not.null) fitms || (not.null) sitms) $ do 
        let xformfunc = cairoXform4PageCoordinate geometry pnum 
            ulbbox = unUnion . mconcat . fmap (Union .Middle . flip inflate 5 . getBBox) $ fitms
            renderfunc = do   
              xformfunc 
              case ulbbox of 
                Top -> do 
                  cairoRenderOption (InBBoxOption Nothing) (InBBox page) 
                  mapM_ renderSelectedItem hitteditms
                Middle sbbox -> do 
                  let redrawee = filter (do2BBoxIntersect sbbox.getBBox) hitteditms  
                  cairoRenderOption (InBBoxOption (Just sbbox)) (InBBox page)
                  clipBBox (Just sbbox)
                  mapM_ renderSelectedItem redrawee 
                Bottom -> return ()
              mapM_ renderSelectedItem sitms 
        liftIO $ updateTempRender tempselection renderfunc False
      when willUpdate $  
        invalidateTemp cid (tempSurfaceSrc tempselection) 
                           (renderBoxSelection bbox) 
      newSelectRectangle cid pnum geometry itms orig 
                         (ncoord,ntime)
                         tempselection { tempInfo = hitteditms }
    upact xstate cinfo pcoord = do       
      let (_,(x,y)) = runIdentity $ 
            skipIfNotInSamePage pnum geometry pcoord 
                                (return (pcoord,prev)) return
          epage = getCurrentPageEitherFromHoodleModeState cinfo (view hoodleModeState xstate)
          cpn = view currentPageNum cinfo 
          bbox = BBox orig (x,y)
          hittestbbox = hltEmbeddedByBBox bbox itms
          selectitms = fmapAL unNotHitted id hittestbbox
          SelectState thdl = view hoodleModeState xstate
          newpage = case epage of 
                      Left pagebbox -> makePageSelectMode pagebbox selectitms 
                      Right tpage -> 
                        let currlayer = view (glayers.selectedLayer) tpage
                            newlayer = set gitems (TEitherAlterHitted (Right selectitms)) currlayer  
                            npage = set (glayers.selectedLayer) newlayer tpage 
                        in npage
          newthdl = set gselSelected (Just (cpn,newpage)) thdl 
          ui = view gtkUIManager xstate
      liftIO $ toggleCutCopyDelete ui (isAnyHitted  selectitms)
      put . set hoodleModeState (SelectState newthdl) 
            =<< (liftIO (updatePageAll (SelectState newthdl) xstate))
      -- invalidateAll 
      invalidateAllInBBox Nothing Efficient


-- | prepare for moving selection 
startMoveSelect :: CanvasId 
                   -> PageNum 
                   -> CanvasGeometry 
                   -> ((Double,Double),UTCTime) 
                   -> Page SelectMode
                   -> MainCoroutine () 
startMoveSelect cid pnum geometry ((x,y),ctime) tpage = do  
    itmimage <- liftIO $ mkItmsNImg geometry tpage
    tsel <- createTempRender pnum geometry
              (hPage2RPage tpage) 
              itmimage 
    moveSelect cid pnum geometry (x,y) ((x,y),ctime) tsel 
    surfaceFinish (tempSurfaceSrc tsel)
    surfaceFinish (tempSurfaceTgt tsel)
    surfaceFinish (imageSurface itmimage)
    -- invalidateAll 
    invalidateAllInBBox Nothing Efficient 

-- | 
moveSelect :: CanvasId
              -> PageNum -- ^ starting pagenum 
              -> CanvasGeometry
              -> (Double,Double)
              -> ((Double,Double),UTCTime)
              -> TempRender ItmsNImg
              -> MainCoroutine ()
moveSelect cid pnum geometry orig@(x0,y0) 
           (prev,otime) tempselection = do
    xst <- get
    r <- nextevent 
    unboxAct (fsingle r xst) . getCanvasInfo cid $ xst 
  where 
    fsingle r xstate cinfo = 
      penMoveAndUpInterPage r pnum geometry defact (moveact xstate cinfo) (upact xstate cinfo) 
    defact = moveSelect cid pnum geometry orig (prev,otime) 
               tempselection
    moveact _xstate _cinfo oldpgn pcpair@(newpgn,PageCoord (px,py)) = do 
      let (x,y) 
            | oldpgn == newpgn = (px,py) 
            | otherwise = 
              let DeskCoord (xo,yo) = page2Desktop geometry (oldpgn,PageCoord (0,0))
                  DeskCoord (xn,yn) = page2Desktop geometry pcpair 
              in (xn-xo,yn-yo)
      
      (willUpdate,(ncoord,ntime)) <- liftIO $ getNewCoordTime (prev,otime) (x,y) 
      when willUpdate $ do 
        let sfunc = offsetFunc (x-x0,y-y0)
            xform = unCvsCoord . desktop2Canvas geometry
                    . page2Desktop geometry . (,) pnum . PageCoord
            (c1,c2) = xform (sfunc (0,0))
            (a1',a2') = xform (sfunc (1,0))
            (a1,a2) = (a1'-c1,a2'-c2)
            (b1',b2') = xform (sfunc (0,1))
            (b1,b2) = (b1'-c1,b2'-c2)
            xformmat = Mat.Matrix a1 a2 b1 b2 c1 c2 
        invalidateTempBasePage cid (tempSurfaceSrc tempselection) pnum 
          (drawTempSelectImage geometry tempselection xformmat) 
      moveSelect cid pnum geometry orig (ncoord,ntime) tempselection
    upact :: HoodleState -> CanvasInfo a -> PointerCoord -> MainCoroutine () 
    upact xst cinfo pcoord =  
      switchActionEnteringDiffPage pnum geometry pcoord (return ()) 
        (chgaction xst cinfo) 
        (ordaction xst cinfo)
    chgaction :: HoodleState -> CanvasInfo a -> PageNum -> (PageNum,PageCoordinate) -> MainCoroutine () 
    chgaction xstate cinfo oldpgn (newpgn,PageCoord (x,y)) = do 
      let hdlmodst@(SelectState thdl) = view hoodleModeState xstate
          epage = getCurrentPageEitherFromHoodleModeState cinfo hdlmodst
      (xstate1,nthdl1,selecteditms) <- 
        case epage of 
          Right oldtpage -> do 
            let itms = getSelectedItms oldtpage
            let oldtpage' = deleteSelected oldtpage
            nthdl <- liftIO $ updateTempHoodleSelectIO thdl oldtpage' (unPageNum oldpgn)
            xst <- return . set hoodleModeState (SelectState nthdl)
                     =<< (liftIO (updatePageAll (SelectState nthdl) xstate)) 
            return (xst,nthdl,itms)       
          Left _ -> error "this is impossible, in moveSelect" 
      let maction = do 
            page <- M.lookup (unPageNum newpgn) (view gselAll nthdl1)
            let currlayer = getCurrentLayer page
            let olditms = view gitems currlayer
            let newitms = map (changeItemBy (offsetFunc (x-x0,y-y0))) selecteditms 
                alist = olditms :- Hitted newitms :- Empty 
                ntpage = makePageSelectMode page alist  
                coroutineaction = do 
                  nthdl2 <- liftIO $ updateTempHoodleSelectIO nthdl1 ntpage (unPageNum newpgn)  
                  let cibox = view currentCanvasInfo xstate1 
                      ncibox = insideAction4CvsInfoBox (set currentPageNum (unPageNum newpgn)) cibox 
                      cmap = getCanvasInfoMap xstate1 
                      cmap' = M.adjust (const ncibox) cid cmap 
                      xst = maybe xstate1 id $ setCanvasInfoMap cmap' xstate1
                  return . set hoodleModeState (SelectState nthdl2)
                    =<< (liftIO (updatePageAll (SelectState nthdl2) xst)) 
            return coroutineaction
      xstate2 <- maybe (return xstate1) id maction 
      commit xstate2
      -- invalidateAll 
      invalidateAllInBBox Nothing Efficient
    ----
    ordaction xstate cinfo _pgn (_cpn,PageCoord (x,y)) = do 
      let offset = (x-x0,y-y0)
          hdlmodst@(SelectState thdl) = view hoodleModeState xstate
          epage = getCurrentPageEitherFromHoodleModeState cinfo hdlmodst
          pagenum = view currentPageNum cinfo
      case epage of 
        Right tpage -> do 
          let newtpage = changeSelectionByOffset offset tpage 
          newthdl <- liftIO $ updateTempHoodleSelectIO thdl newtpage pagenum 
          commit . set hoodleModeState (SelectState newthdl)
                 =<< (liftIO (updatePageAll (SelectState newthdl) xstate))
        Left _ -> error "this is impossible, in moveSelect" 
      -- invalidateAll 
      invalidateAllInBBox Nothing Efficient 
      

-- | prepare for resizing selection 
startResizeSelect :: Handle 
                     -> CanvasId 
                     -> PageNum 
                     -> CanvasGeometry 
                     -> BBox
                     -> ((Double,Double),UTCTime) 
                     -> Page SelectMode
                     -> MainCoroutine () 
startResizeSelect handle cid pnum geometry bbox 
                  ((x,y),ctime) tpage = do  
    itmimage <- liftIO $ mkItmsNImg geometry tpage  
    tsel <- createTempRender pnum geometry 
              (hPage2RPage tpage) 
              itmimage 
    resizeSelect handle cid pnum geometry bbox ((x,y),ctime) tsel 
    surfaceFinish (tempSurfaceSrc tsel)  
    surfaceFinish (tempSurfaceTgt tsel)      
    surfaceFinish (imageSurface itmimage)
    -- invalidateAll 
    invalidateAllInBBox Nothing Efficient

-- | 
resizeSelect :: Handle 
                -> CanvasId
                -> PageNum 
                -> CanvasGeometry
                -> BBox
                -> ((Double,Double),UTCTime)
                -> TempRender ItmsNImg
                -> MainCoroutine ()
resizeSelect handle cid pnum geometry origbbox 
             (prev,otime) tempselection = do
    xst <- get
    r <- nextevent 
    unboxAct (fsingle r xst) . getCanvasInfo cid $ xst
  where
    fsingle r xstate cinfo = penMoveAndUpOnly r pnum geometry defact (moveact xstate cinfo) (upact xstate cinfo)
    defact = resizeSelect handle cid pnum geometry 
               origbbox (prev,otime) tempselection
    moveact _xstate _cinfo (_pcoord,(x,y)) = do 
      (willUpdate,(ncoord,ntime)) <- liftIO $ getNewCoordTime (prev,otime) (x,y) 
      when willUpdate $ do 
        let newbbox = getNewBBoxFromHandlePos handle origbbox (x,y)      
            sfunc = scaleFromToBBox origbbox newbbox
            xform = unCvsCoord . desktop2Canvas geometry
                    . page2Desktop geometry . (,) pnum . PageCoord
            (c1,c2) = xform (sfunc (0,0))
            (a1',a2') = xform (sfunc (1,0))
            (a1,a2) = (a1'-c1,a2'-c2)
            (b1',b2') = xform (sfunc (0,1))
            (b1,b2) = (b1'-c1,b2'-c2)
            xformmat = Mat.Matrix a1 a2 b1 b2 c1 c2 
        invalidateTemp cid (tempSurfaceSrc tempselection) 
                           (drawTempSelectImage geometry tempselection 
                              xformmat)
      resizeSelect handle cid pnum geometry 
                   origbbox (ncoord,ntime) tempselection
    upact xstate cinfo pcoord = do 
      let (_,(x,y)) = runIdentity $ 
            skipIfNotInSamePage pnum geometry pcoord 
                                (return (pcoord,prev)) return
          newbbox = getNewBBoxFromHandlePos handle origbbox (x,y)
          hdlmodst@(SelectState thdl) = view hoodleModeState xstate
          epage = getCurrentPageEitherFromHoodleModeState cinfo hdlmodst
          pagenum = view currentPageNum cinfo
      case epage of 
        Right tpage -> do 
          let sfunc = scaleFromToBBox origbbox newbbox
          let newtpage = changeSelectionBy sfunc tpage 
          newthdl <- liftIO $ updateTempHoodleSelectIO thdl newtpage pagenum 
          commit . set hoodleModeState (SelectState newthdl)
                 =<< (liftIO (updatePageAll (SelectState newthdl) xstate))
        Left _ -> error "this is impossible, in resizeSelect" 
      -- invalidateAll
      invalidateAllInBBox Nothing Efficient
      return ()    

  
-- | 
selectPenColorChanged :: PenColor -> MainCoroutine () 
selectPenColorChanged pcolor = do 
  xstate <- get
  let SelectState thdl = view hoodleModeState xstate 
      Just (n,tpage) = view gselSelected thdl
      slayer = view (glayers.selectedLayer) tpage
  case unTEitherAlterHitted . view gitems $ slayer of 
    Left _ -> return () 
    Right alist -> do 
      let alist' = fmapAL id 
                     (Hitted . map (changeItemStrokeColor pcolor) . unHitted) alist
          newlayer = Right alist'
          newpage = set (glayers.selectedLayer) (GLayer (view gbuffer slayer) (TEitherAlterHitted newlayer)) tpage 
      newthdl <- liftIO $ updateTempHoodleSelectIO thdl newpage n
      commit =<< liftIO (updatePageAll (SelectState newthdl)
                        . set hoodleModeState (SelectState newthdl) $ xstate )
      -- invalidateAll 
      invalidateAllInBBox Nothing Efficient
          
-- | 
selectPenWidthChanged :: Double -> MainCoroutine () 
selectPenWidthChanged pwidth = do 
  xstate <- get
  let SelectState thdl = view hoodleModeState xstate 
      Just (n,tpage) = view gselSelected thdl
      slayer = view (glayers.selectedLayer) tpage
  case unTEitherAlterHitted . view gitems $ slayer  of 
    Left _ -> return () 
    Right alist -> do 
      let alist' = fmapAL id 
                     (Hitted . map (changeItemStrokeWidth pwidth) . unHitted) alist
          newlayer = Right alist'
          newpage = set (glayers.selectedLayer) (GLayer (view gbuffer slayer) (TEitherAlterHitted newlayer)) tpage
      newthdl <- liftIO $ updateTempHoodleSelectIO thdl newpage n          
      commit =<< liftIO (updatePageAll (SelectState newthdl) 
                         . set hoodleModeState (SelectState newthdl) $ xstate )
      -- invalidateAll 
      invalidateAllInBBox Nothing Efficient 

-- | main mouse pointer click entrance in lasso selection mode. 
--   choose either starting new rectangular selection or move previously 
--   selected selection. 
selectLassoStart :: PenButton -> CanvasId -> PointerCoord -> MainCoroutine ()
selectLassoStart pbtn cid = commonPenStart lassoAction cid 
  where lassoAction cinfo pnum geometry (x,y) = do 
          itms <- rItmsInCurrLyr
          ctime <- liftIO $ getCurrentTime
          let newSelectAction page =    
                dealWithOneTimeSelectMode 
                  (do tsel <- createTempRender pnum geometry page [] 
                      newSelectLasso cinfo pnum geometry itms 
                                     (x,y) ((x,y),ctime) (Sq.empty |> (x,y)) tsel
                      surfaceFinish (tempSurfaceSrc tsel)
                      surfaceFinish (tempSurfaceTgt tsel)
                      showContextMenu (pnum,(x,y))
                  )
                  (return ()) 
          let action (Right tpage) | hitInSelection tpage (x,y) = 
                case pbtn of
                  PenButton1 -> startMoveSelect cid pnum geometry ((x,y),ctime) tpage
                  PenButton3 -> do 
                    waitSomeEvent (\e -> case e of PenUp _ _ -> True ; _ -> False) 
                    showContextMenu (pnum,(x,y))                    
                  _ -> return () 
              action (Right tpage) | hitInHandle tpage (x,y) = 
                case getULBBoxFromSelected tpage of 
                  Middle bbox ->  
                    maybe (return ()) 
                          (\handle -> startResizeSelect 
                                        handle cid pnum geometry 
                                        bbox ((x,y),ctime) tpage)
                          (checkIfHandleGrasped bbox (x,y))
                  _ -> return () 
              action (Right tpage) | otherwise = (newSelectAction . hPage2RPage) tpage 
              action (Left page) = newSelectAction page
          xstate <- get 
          let hdlmodst = view hoodleModeState xstate 
          let epage = getCurrentPageEitherFromHoodleModeState cinfo hdlmodst 
          action epage
          
-- | 
newSelectLasso :: CanvasInfo a
                  -> PageNum 
                  -> CanvasGeometry
                  -> [RItem] 
                  -> (Double,Double)
                  -> ((Double,Double),UTCTime)
                  -> Seq (Double,Double)
                  -> TempSelection 
                  -> MainCoroutine ()
newSelectLasso cvsInfo pnum geometry itms orig (prev,otime) lasso tsel = nextevent >>= flip fsingle cvsInfo 
  where  
    fsingle r cinfo = penMoveAndUpOnly r pnum geometry defact
                        (moveact cinfo) (upact cinfo)
    defact = newSelectLasso cvsInfo pnum geometry itms orig 
               (prev,otime) lasso tsel
    moveact cinfo (_pcoord,(x,y)) = do 
      let nlasso = lasso |> (x,y)
      (willUpdate,(ncoord,ntime)) <- liftIO $ getNewCoordTime (prev,otime) (x,y)
      when willUpdate $ do 
        invalidateTemp (view canvasId cinfo) (tempSurfaceSrc tsel) (renderLasso geometry nlasso) 
      newSelectLasso cinfo pnum geometry itms orig (ncoord,ntime) nlasso tsel
    upact cinfo pcoord = do 
      xstate <- get 
      let (_,(x,y)) = runIdentity $ 
            skipIfNotInSamePage pnum geometry pcoord 
                                (return (pcoord,prev)) return
          nlasso = lasso |> (x,y)
          hdlmodst = view hoodleModeState xstate 
          epage = getCurrentPageEitherFromHoodleModeState cinfo hdlmodst
          cpn = view currentPageNum cinfo 
          hittestlasso1 = hltFilteredBy (hitLassoItem (nlasso |> orig)) itms
          selectitms1 = fmapAL unNotHitted id hittestlasso1
          selecteditms1 = (concatMap unHitted . getB) selectitms1 
          hittestlasso2 = takeLastFromHitted . flip hltFilteredBy itms $ 
                            \itm-> (not.isStrkInRItem) itm 
                                   && isPointInBBox (getBBox itm) (x,y)
          selectitms2 = fmapAL unNotHitted id hittestlasso2
          selectitms 
            | (not.null) selecteditms1 = selectitms1 
            | otherwise = selectitms2
          SelectState thdl = view hoodleModeState xstate
          newpage = case epage of 
                      Left pagebbox -> 
                        let currlayer= getCurrentLayer pagebbox
                            newlayer = GLayer (view gbuffer currlayer) (TEitherAlterHitted (Right selectitms))
                            tpg = mkHPage pagebbox 
                            npg = set (glayers.selectedLayer) newlayer tpg
                        in npg 
                      Right tpage -> 
                        let currlayer = view (glayers.selectedLayer) tpage
                            newlayer = GLayer (view gbuffer currlayer) (TEitherAlterHitted (Right selectitms))
                            npage = set (glayers.selectedLayer) newlayer tpage 
                        in npage
          newthdl = set gselSelected (Just (cpn,newpage)) thdl 
      let ui = view gtkUIManager xstate
      liftIO $ toggleCutCopyDelete ui (isAnyHitted  selectitms)
      put . set hoodleModeState (SelectState newthdl) 
            =<< (liftIO (updatePageAll (SelectState newthdl) xstate))
      -- invalidateAll 
      invalidateAllInBBox Nothing Efficient


