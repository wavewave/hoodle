{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Hoodle.Coroutine.Page where

import Control.Concurrent.STM (atomically)
import Control.Lens (set, view, (.~), (?~), (^.))
import Control.Monad (when)
import Control.Monad.State (get, gets, liftIO, put)
import Control.Monad.Trans.Reader (ask)
import Data.Either (fromRight)
import qualified Data.Foldable as F
import Data.Function (on)
import Data.Hoodle.Generic
  ( gbackground,
    gbuffer,
    gdimension,
    gembeddedpdf,
    gitems,
    glayers,
    gpages,
    pdfNumPages,
  )
import Data.Hoodle.Select (gselAll)
import Data.Hoodle.Simple (Dimension (..))
import Data.Hoodle.Zipper (fromNonEmptyList)
import qualified Data.IntMap as M
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Graphics.Hoodle.Render.Type
  ( GenCommand (BkgSmplScaled, LayerScaled),
    LyBuf (..),
    PDFCommand (RenderPageScaled),
    RBackground (..),
    RHoodle,
    RLayer,
    Renderer,
    RendererState (..),
    emptyRLayer,
    issueGenCommandID,
    issuePDFCommandID,
    issueSurfaceID,
    sendGenCommand,
    sendPDFCommand,
  )
import Hoodle.Accessor
  ( getCurrentPageCvsId,
    getGeometry4CurrCvs,
    updateUhdl,
  )
import Hoodle.Coroutine.Commit (commit_)
import Hoodle.Coroutine.Draw
  ( callRenderer_,
    invalidateAll,
    invalidateAllInBBox,
    invalidateInBBox,
  )
import Hoodle.Coroutine.Scroll
  ( adjustScrollbarWithGeometryCurrent,
    adjustScrollbarWithGeometryCvsId,
  )
import Hoodle.ModelAction.Page (relZoomRatio, setPage, updatePageAll)
import Hoodle.Type.Alias (EditMode, Hoodle, Page)
import Hoodle.Type.Canvas
  ( CanvasId,
    CanvasInfo (..),
    CanvasInfoBox (CanvasContPage, CanvasSinglePage),
    canvasId,
    currentPageNum,
    drawArea,
    forBoth',
    pageArrangement,
    unboxBiAct,
    unboxLens,
    viewInfo,
    zoomMode,
  )
import Hoodle.Type.Coroutine (MainCoroutine)
import Hoodle.Type.Enum
  ( AddDirection (PageAfter, PageBefore),
    BackgroundStyle,
    DrawFlag (Efficient),
    NewPageModeType (NPCycle, NPLast, NPPlain),
    ZoomModeRel,
    convertBackgroundStyleToByteString,
  )
import Hoodle.Type.HoodleState
  ( HoodleModeState (SelectState, ViewAppendState),
    UnitHoodle,
    backgroundStyle,
    currentCanvasInfo,
    currentUnit,
    cvsInfoMap,
    getCanvasInfo,
    getCurrentCanvasId,
    getHoodle,
    hoodleModeState,
    hoodleModeStateEither,
    modifyCanvasInfo,
    newPageMode,
    nextPdfBkgPageNum,
    settings,
    unitHoodles,
  )
import Hoodle.Type.PageArrangement
  ( CanvasCoordinate (..),
    PageCoordinate (..),
    PageDimension (..),
    PageNum (..),
    ZoomMode (Zoom),
    makeContinuousArrangement,
    makeSingleArrangement,
  )
import Hoodle.Util (maybeError', msgShout)
import Hoodle.View.Coordinate
  ( CanvasGeometry,
    canvasDim,
    desktop2Canvas,
    getCvsGeomFrmCvsInfo,
    getCvsOriginInPage,
    makeCanvasGeometry,
    page2Desktop,
  )

-- | change page of current canvas using a modify function
changePage :: (Int -> Int) -> MainCoroutine ()
changePage modifyfn =
  gets (view backgroundStyle) >>= \bsty ->
    updateUhdl (changePageAction bsty)
      >> adjustScrollbarWithGeometryCurrent
      >> invalidateAllInBBox Nothing Efficient
  where
    changePageAction bsty uhdl =
      unboxBiAct (fsingle bsty uhdl) (fcont bsty uhdl)
        . (^. currentCanvasInfo)
        $ uhdl
    fsingle bsty uhdl cvsInfo = do
      let xojst = view hoodleModeState uhdl
          npgnum = modifyfn (cvsInfo ^. currentPageNum)
          cid = view canvasId cvsInfo
      (b, npgnum', _, xojst') <- changePageInHoodleModeState bsty npgnum xojst
      uhdl' <- liftIO $ updatePageAll xojst' uhdl
      ncvsInfo <- liftIO $ setPage uhdl' (PageNum npgnum') cid
      let uhdlfinal = (currentCanvasInfo .~ ncvsInfo) uhdl'
      when b $ updateUhdl (const (return uhdlfinal)) >> commit_
      return uhdlfinal
    fcont bsty uhdl cvsInfo = do
      let xojst = view hoodleModeState uhdl
          npgnum = modifyfn (cvsInfo ^. currentPageNum)
          cid = cvsInfo ^. canvasId
      (b, npgnum', _, xojst') <- changePageInHoodleModeState bsty npgnum xojst
      uhdl' <- liftIO $ updatePageAll xojst' uhdl
      ncvsInfo <- liftIO $ setPage uhdl' (PageNum npgnum') cid
      let uhdlfinal = (currentCanvasInfo .~ ncvsInfo) uhdl'
      when b $ updateUhdl (const (return uhdlfinal)) >> commit_
      return uhdlfinal

-- |
changePageInHoodleModeState ::
  BackgroundStyle ->
  -- | new page number
  Int ->
  HoodleModeState ->
  MainCoroutine (Bool, Int, Page EditMode, HoodleModeState)
changePageInHoodleModeState bsty npgnum hdlmodst = do
  let ehdl = hoodleModeStateEither hdlmodst
      pgs = either (view gpages) (view gselAll) ehdl
      totnumpages = M.size pgs
      lpage = maybeError' "changePage" (M.lookup (totnumpages - 1) pgs)
  (isChanged, npgnum', npage', ehdl') <-
    if npgnum >= totnumpages
      then do
        let cbkg = view gbackground lpage
        nbkg <- newBkg bsty cbkg
        npage <- set gbackground nbkg <$> newPageFromOld lpage
        geometry <- liftIO . getGeometry4CurrCvs . view (unitHoodles . currentUnit) =<< get
        callRenderer_ $ updatePageCache geometry (PageNum (totnumpages - 1), npage)
        let npages = M.insert totnumpages npage pgs
        return
          ( True,
            totnumpages,
            npage,
            either (Left . set gpages npages) (Right . set gselAll npages) ehdl
          )
      else do
        let npg = if npgnum < 0 then 0 else npgnum
            pg = maybeError' "changePage" (M.lookup npg pgs)
        return (False, npg, pg, ehdl)
  return (isChanged, npgnum', npage', either ViewAppendState SelectState ehdl')

-- |
canvasZoomUpdateGenRenderCvsId ::
  MainCoroutine () ->
  CanvasId ->
  Maybe ZoomMode ->
  Maybe (PageNum, PageCoordinate) ->
  MainCoroutine ()
canvasZoomUpdateGenRenderCvsId renderfunc cid mzmode mcoord = do
  updateUhdl zoomUpdateAction
  adjustScrollbarWithGeometryCvsId cid
  xst <- get
  let uhdl = view (unitHoodles . currentUnit) xst
      hdl = getHoodle uhdl
  geometry <- liftIO (getGeometry4CurrCvs uhdl)
  let cpn = view (unboxLens currentPageNum) . getCanvasInfo cid $ uhdl
  let plst = sortBy (compare `on` (\(n, _) -> abs (n - cpn))) . zip [0 ..] . F.toList $ hdl ^. gpages
  F.forM_ plst $ \(pn, pg) -> callRenderer_ (updatePageCache geometry (PageNum pn, pg))
  renderfunc
  where
    zoomUpdateAction uhdl =
      unboxBiAct (fsingle uhdl) (fcont uhdl) . getCanvasInfo cid $ uhdl
    fsingle uhdl cinfo = do
      geometry <- liftIO $ getCvsGeomFrmCvsInfo cinfo
      page <- getCurrentPageCvsId cid
      let zmode = fromMaybe (cinfo ^. viewInfo . zoomMode) mzmode
          pdim = PageDimension $ page ^. gdimension
          xy =
            either
              (const (0, 0))
              (unPageCoord . snd)
              (getCvsOriginInPage geometry)
          cdim = canvasDim geometry
          narr = makeSingleArrangement zmode pdim cdim xy
          ncinfobox =
            CanvasSinglePage
              . (viewInfo . pageArrangement .~ narr)
              . (viewInfo . zoomMode .~ zmode)
              $ cinfo
      return . modifyCanvasInfo cid (const ncinfobox) $ uhdl
    fcont uhdl cinfo = do
      geometry <- liftIO $ getCvsGeomFrmCvsInfo cinfo
      let zmode = fromMaybe (view (viewInfo . zoomMode) cinfo) mzmode
          cpn = PageNum $ view currentPageNum cinfo
          cdim = canvasDim geometry
          hdl = getHoodle uhdl
          origcoord = case mcoord of
            Just coord -> coord
            Nothing ->
              fromRight (cpn, PageCoord (0, 0)) (getCvsOriginInPage geometry)
          narr = makeContinuousArrangement zmode cdim hdl origcoord
          ncinfobox =
            CanvasContPage
              . (viewInfo . pageArrangement .~ narr)
              . (viewInfo . zoomMode .~ zmode)
              $ cinfo
      return . modifyCanvasInfo cid (const ncinfobox) $ uhdl

-- |
canvasZoomUpdateCvsId ::
  CanvasId ->
  Maybe ZoomMode ->
  MainCoroutine ()
canvasZoomUpdateCvsId cid mzmode =
  canvasZoomUpdateGenRenderCvsId invalidateAll cid mzmode Nothing

-- |
canvasZoomUpdateBufAll :: MainCoroutine ()
canvasZoomUpdateBufAll = do
  klst <- gets (M.keys . view cvsInfoMap . view (unitHoodles . currentUnit))
  mapM_ updatefunc klst
  where
    updatefunc cid =
      canvasZoomUpdateGenRenderCvsId (invalidateInBBox Nothing Efficient cid) cid Nothing Nothing

-- |
canvasZoomUpdateAll :: MainCoroutine ()
canvasZoomUpdateAll = do
  klst <- gets (M.keys . view cvsInfoMap . view (unitHoodles . currentUnit))
  mapM_ (`canvasZoomUpdateCvsId` Nothing) klst

-- |
canvasZoomUpdate :: Maybe ZoomMode -> MainCoroutine ()
canvasZoomUpdate mzmode = do
  cid <- gets (getCurrentCanvasId . view (unitHoodles . currentUnit))
  canvasZoomUpdateCvsId cid mzmode

-- |
pageZoomChange :: ZoomMode -> MainCoroutine ()
pageZoomChange = canvasZoomUpdate . Just

-- |
pageZoomChangeRel :: ZoomModeRel -> MainCoroutine ()
pageZoomChangeRel rzmode = do
  forBoth' unboxBiAct fsingle . view currentCanvasInfo . view (unitHoodles . currentUnit) =<< get
  where
    fsingle :: CanvasInfo a -> MainCoroutine ()
    fsingle cinfo = do
      let cpn = PageNum (cinfo ^. currentPageNum)
          arr = cinfo ^. viewInfo . pageArrangement
          canvas = cinfo ^. drawArea
      geometry <- liftIO $ makeCanvasGeometry cpn arr canvas
      let nratio = relZoomRatio geometry rzmode
      pageZoomChange (Zoom nratio)

-- |
newPage :: Maybe Dimension -> AddDirection -> MainCoroutine ()
newPage mdim dir =
  gets (view backgroundStyle) >>= \bsty ->
    updateUhdl (npgBfrAct bsty)
      >> commit_
      >> canvasZoomUpdateAll
      >> invalidateAll
  where
    npgBfrAct bsty uhdl = forBoth' unboxBiAct (fsimple bsty uhdl) . view currentCanvasInfo $ uhdl
    fsimple :: BackgroundStyle -> UnitHoodle -> CanvasInfo a -> MainCoroutine UnitHoodle
    fsimple bsty uhdl cinfo = do
      case view hoodleModeState uhdl of
        ViewAppendState hdl -> do
          hdl' <- addNewPageInHoodle mdim bsty dir hdl (view currentPageNum cinfo)
          liftIO . updatePageAll (ViewAppendState hdl')
            . set hoodleModeState (ViewAppendState hdl')
            $ uhdl
        SelectState _ -> do
          msgShout "newPage: not implemented yet"
          return uhdl

-- | delete current page of current canvas
deleteCurrentPage :: MainCoroutine ()
deleteCurrentPage =
  updateUhdl delpgact
    >> commit_
    >> canvasZoomUpdateAll
    >> invalidateAll
  where
    delpgact uhdl = forBoth' unboxBiAct (fsimple uhdl) . view currentCanvasInfo $ uhdl
    fsimple :: UnitHoodle -> CanvasInfo a -> MainCoroutine UnitHoodle
    fsimple uhdl cinfo = do
      case view hoodleModeState uhdl of
        ViewAppendState hdl -> do
          hdl' <-
            liftIO $
              deletePageInHoodle
                hdl
                (PageNum (view currentPageNum cinfo))
          liftIO . updatePageAll (ViewAppendState hdl')
            . set hoodleModeState (ViewAppendState hdl')
            $ uhdl
        SelectState _ -> do
          msgShout "deleteCurrentPage: not implemented yet"
          return uhdl

-- | delete designated page
deletePageInHoodle :: Hoodle EditMode -> PageNum -> IO (Hoodle EditMode)
deletePageInHoodle hdl (PageNum pgn) = do
  let pagelst = M.elems . view gpages $ hdl
      (pagesbefore, _cpage : pagesafter) = splitAt pgn pagelst
      npagelst = pagesbefore ++ pagesafter
      nhdl = set gpages (M.fromList . zip [0 ..] $ npagelst) hdl
  return nhdl

-- |
addNewPageInHoodle ::
  Maybe Dimension ->
  BackgroundStyle ->
  AddDirection ->
  Hoodle EditMode ->
  Int ->
  MainCoroutine (Hoodle EditMode)
addNewPageInHoodle mdim bsty dir hdl cpn = do
  let pagelst = M.elems . view gpages $ hdl
      (pagesbefore, cpage : pagesafter) = splitAt cpn pagelst
      cbkg = view gbackground cpage
  nbkg <- newBkg bsty cbkg
  npage <- maybe id (set gdimension) mdim . set gbackground nbkg <$> newPageFromOld cpage
  geometry <- liftIO . getGeometry4CurrCvs . view (unitHoodles . currentUnit) =<< get
  callRenderer_ (updatePageCache geometry (PageNum cpn, npage))
  let npagelst = case dir of
        PageBefore -> pagesbefore ++ (npage : cpage : pagesafter)
        PageAfter -> pagesbefore ++ (cpage : npage : pagesafter)
      nhdl = set gpages (M.fromList . zip [0 ..] $ npagelst) hdl
  return nhdl

newBkg :: BackgroundStyle -> RBackground -> MainCoroutine RBackground
newBkg bsty bkg = do
  xst <- get
  let npmode = xst ^. settings . newPageMode
      rhdl = (getHoodle . view (unitHoodles . currentUnit)) xst
      mtotN = pdfNumPages <$> (rhdl ^. gembeddedpdf)
  let bstystr = convertBackgroundStyleToByteString bsty
      defbkg = RBkgSmpl "white" bstystr <$> issueSurfaceID
  case npmode of
    NPPlain -> defbkg
    NPLast -> case bkg of
      RBkgSmpl c _ _ -> RBkgSmpl c bstystr <$> issueSurfaceID
      RBkgPDF d f n pg _ -> RBkgPDF d f n pg <$> issueSurfaceID
      RBkgEmbedPDF n pg _ -> RBkgEmbedPDF n pg <$> issueSurfaceID
    NPCycle ->
      case mtotN of
        Nothing -> defbkg
        Just totN -> do
          let n1 = fromMaybe 1 (xst ^. nextPdfBkgPageNum)
          case findPDFBkg rhdl n1 of
            Nothing -> defbkg
            Just bkg' ->
              issueSurfaceID >>= \i -> do
                let n' = if n1 >= totN then 1 else n1 + 1
                put ((nextPdfBkgPageNum ?~ n') xst)
                return bkg' {rbkg_surfaceid = i}

findPDFBkg :: RHoodle -> Int -> Maybe RBackground
findPDFBkg rhdl n1 =
  let bkgs = M.elems (rhdl ^. gpages)
      pagematch n (RBkgPDF _ _ n' _ _) = n == n'
      pagematch n (RBkgEmbedPDF n' _ _) = n == n'
      pagematch _ _ = False
      matched = (filter (pagematch n1) . map (^. gbackground)) bkgs
   in case matched of
        [] -> Nothing
        b : _ -> Just b

-- |
newPageFromOld :: Page EditMode -> MainCoroutine (Page EditMode)
newPageFromOld pg = do
  sfcid <- issueSurfaceID
  return . (glayers .~ fromNonEmptyList (emptyRLayer sfcid, [])) $ pg

updatePageCache :: CanvasGeometry -> (PageNum, Page EditMode) -> Renderer ()
updatePageCache geometry (pnum, page) = do
  let Dim w h = page ^. gdimension
      CvsCoord (x0, y0) =
        (desktop2Canvas geometry . page2Desktop geometry) (pnum, PageCoord (0, 0))
      CvsCoord (x1, y1) =
        (desktop2Canvas geometry . page2Desktop geometry) (pnum, PageCoord (w, h))
  updateBkgCache (Dim w h) (Dim (x1 - x0) (y1 - y0)) (page ^. gbackground)
  mapM_ (updateLayerCache (Dim w h) (Dim (x1 - x0) (y1 - y0))) (F.toList (page ^. glayers))

updateBkgCache :: Dimension -> Dimension -> RBackground -> Renderer ()
updateBkgCache dimo dimv rbkg = do
  RendererState _handler qpdf qgen _ <- ask
  let sfcid = rbkg_surfaceid rbkg
  case rbkg of
    RBkgSmpl {..} -> do
      cmdid <- issueGenCommandID
      (liftIO . atomically) (sendGenCommand qgen cmdid (BkgSmplScaled sfcid rbkg_color rbkg_style dimo dimv))
      return ()
    _ -> F.forM_ (rbkg_popplerpage rbkg) $ \pg -> do
      cmdid <- issuePDFCommandID
      (liftIO . atomically) (sendPDFCommand qpdf cmdid (RenderPageScaled sfcid pg dimo dimv))
      return ()

updateLayerCache :: Dimension -> Dimension -> RLayer -> Renderer ()
updateLayerCache dimo dimv lyr = do
  RendererState _ _ qgen _ <- ask
  case view gbuffer lyr of
    LyBuf sfcid -> do
      cmdid <- issueGenCommandID
      (liftIO . atomically) (sendGenCommand qgen cmdid (LayerScaled sfcid (view gitems lyr) dimo dimv))
      return ()
