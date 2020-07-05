{-# LANGUAGE MultiWayIf #-}

module Hoodle.Coroutine.Select where

import Control.Applicative
import Control.Lens ((.~), (^.), set, view)
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Data.Bifunctor (first, second)
import Data.Hoodle.BBox
import Data.Hoodle.Generic
import Data.Hoodle.Select
import qualified Data.IntMap as M
import Data.Monoid
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Sq (empty)
import Data.Time.Clock
import Graphics.Hoodle.Render.Generic
import Graphics.Hoodle.Render.Type
import Graphics.Hoodle.Render.Type.HitTest
import Graphics.Hoodle.Render.Util
import Graphics.Hoodle.Render.Util.HitTest
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Cairo.Matrix as Mat
import Hoodle.Accessor
import Hoodle.Coroutine.Commit
import Hoodle.Coroutine.ContextMenu
import Hoodle.Coroutine.Draw
import Hoodle.Coroutine.Mode
import Hoodle.Coroutine.Pen
import Hoodle.Coroutine.Select.Clipboard
import Hoodle.Device
import Hoodle.ModelAction.Layer
import Hoodle.ModelAction.Page
import Hoodle.ModelAction.Pen
import Hoodle.ModelAction.Select
import Hoodle.ModelAction.Select.Transform
import Hoodle.Type.Alias
import Hoodle.Type.Canvas
import Hoodle.Type.Coroutine
import Hoodle.Type.Enum
import Hoodle.Type.Event
import Hoodle.Type.HoodleState
import Hoodle.Type.PageArrangement
import Hoodle.View.Coordinate
import Hoodle.View.Draw

--
-- import           Prelude hiding ((.), id)

-- | For Selection mode from pen mode with 2nd pen button
dealWithOneTimeSelectMode ::
  -- | main action
  MainCoroutine () ->
  -- | terminating action
  MainCoroutine () ->
  MainCoroutine ()
dealWithOneTimeSelectMode action terminator = do
  uhdl <- view (unitHoodles . currentUnit) <$> get
  case view isOneTimeSelectMode uhdl of
    NoOneTimeSelectMode -> action
    YesBeforeSelect ->
      action >> pureUpdateUhdl (isOneTimeSelectMode .~ YesAfterSelect)
    YesAfterSelect -> do
      terminator
      pureUpdateUhdl (isOneTimeSelectMode .~ NoOneTimeSelectMode)
      modeChange ToViewAppendMode

-- | common main mouse pointer click entrance in selection mode.
--   choose either starting new selection or move previously
--   selected selection.
commonSelectStart ::
  SelectType ->
  PenButton ->
  CanvasId ->
  PointerCoord ->
  MainCoroutine ()
commonSelectStart typ pbtn cid = case typ of
  SelectHandToolWork -> (\_ -> return ())
  _ -> commonPenStart selectaction cid >=> const (return ())
  where
    selectaction cinfo pnum geometry (x, y) _ = do
      itms <- rItmsInCurrLyr
      ctime <- liftIO $ getCurrentTime
      let newSelectAction _page =
            dealWithOneTimeSelectMode
              ( do
                  tsel <- createTempRender geometry []
                  case typ of
                    SelectRectangleWork -> do
                      newSelectRectangle
                        cid
                        pnum
                        geometry
                        itms
                        (x, y)
                        ((x, y), ctime)
                        tsel
                      return ()
                    SelectLassoWork -> do
                      newSelectLasso
                        cinfo
                        pnum
                        geometry
                        itms
                        (x, y)
                        ((x, y), ctime)
                        (Sq.empty |> (x, y))
                        tsel
                      return ()
                    _ -> return ()
                  Cairo.surfaceFinish (tempSurfaceSrc tsel)
                  showContextMenu (pnum, (x, y))
              )
              (return ())
          action (Right tpage) | hitInHandle tpage (x, y) = do
            let doesKeepRatio = case pbtn of
                  PenButton1 -> True
                  PenButton3 -> False
                  _ -> False
            case getULBBoxFromSelected tpage of
              Middle bbox ->
                maybe
                  (return ())
                  ( \handle ->
                      startResizeSelect
                        doesKeepRatio
                        handle
                        cid
                        pnum
                        geometry
                        bbox
                        ((x, y), ctime)
                        tpage
                  )
                  (checkIfHandleGrasped bbox (x, y))
              _ -> return ()
          action (Right tpage) | hitInSelection tpage (x, y) =
            case pbtn of
              PenButton1 -> startMoveSelect cid pnum geometry ((x, y), ctime) tpage
              PenButton3 -> do
                waitSomeEvent (\e -> case e of PenUp _ _ -> True; _ -> False)
                showContextMenu (pnum, (x, y))
              _ -> return ()
          action (Right tpage) | otherwise = newSelectAction (hPage2RPage tpage)
          action (Left page) = newSelectAction page
      uhdl <- view (unitHoodles . currentUnit) <$> get
      let hdlmodst = view hoodleModeState uhdl
      let epage = getCurrentPageEitherFromHoodleModeState cinfo hdlmodst
      action epage

-- | main mouse pointer click entrance in rectangular selection mode.
--   choose either starting new rectangular selection or move previously
--   selected selection.
selectRectStart :: PenButton -> CanvasId -> PointerCoord -> MainCoroutine ()
selectRectStart = commonSelectStart SelectRectangleWork

-- |
newSelectRectangle ::
  CanvasId ->
  PageNum ->
  CanvasGeometry ->
  [RItem] ->
  (Double, Double) ->
  ((Double, Double), UTCTime) ->
  TempSelection ->
  MainCoroutine ()
newSelectRectangle
  cid
  pnum
  geometry
  itms
  orig
  (prev, otime)
  tempselection = do
    r <- nextevent
    xst <- get
    cache <- renderCache
    forBoth' unboxBiAct (fsingle r xst cache) . getCanvasInfo cid . view (unitHoodles . currentUnit) $ xst
    where
      fsingle r xstate cache cinfo = penMoveAndUpOnly r pnum geometry defact (moveact cache) (upact xstate cinfo)
      defact = newSelectRectangle cid pnum geometry itms orig (prev, otime) tempselection
      moveact cache (_pcoord, (x, y)) = do
        let bbox = BBox orig (x, y)
            hittestbbox = hltEmbeddedByBBox bbox itms
            hitteditms = takeHitted hittestbbox
        page <- getCurrentPageCvsId cid
        let (fitms, sitms) = separateFS $ getDiffBBox (tempInfo tempselection) hitteditms
        (willUpdate, (ncoord, ntime)) <- liftIO $ getNewCoordTime (prev, otime) (x, y)
        when ((not . null) fitms || (not . null) sitms) $ do
          let xformfunc = cairoXform4PageCoordinate (mkXform4Page geometry pnum)
              ulbbox = unUnion . mconcat . fmap (Union . Middle . flip inflate 5 . getBBox) $ fitms
              xform = mkXform4Page geometry pnum
              renderfunc = do
                xformfunc
                case ulbbox of
                  Top -> do
                    cairoRenderOption (InBBoxOption Nothing) cache cid (InBBox page, Just xform)
                    mapM_ renderSelectedItem hitteditms
                  Middle sbbox -> do
                    let redrawee = filter (do2BBoxIntersect sbbox . getBBox) hitteditms
                    cairoRenderOption (InBBoxOption (Just sbbox)) cache cid (InBBox page, Just xform)
                    clipBBox (Just sbbox)
                    mapM_ renderSelectedItem redrawee
                  Bottom -> return ()
                mapM_ renderSelectedItem sitms
          liftIO $ updateTempRender tempselection renderfunc False
        when willUpdate $
          invalidateTemp
            cid
            (tempSurfaceSrc tempselection)
            (renderBoxSelection bbox)
        newSelectRectangle
          cid
          pnum
          geometry
          itms
          orig
          (ncoord, ntime)
          tempselection {tempInfo = hitteditms}
      upact xstate cinfo pcoord = do
        let (_, (x, y)) =
              runIdentity $
                skipIfNotInSamePage
                  pnum
                  geometry
                  pcoord
                  (return (pcoord, prev))
                  return
            uhdl = view (unitHoodles . currentUnit) xstate
            epage = getCurrentPageEitherFromHoodleModeState cinfo (view hoodleModeState uhdl)
            cpn = view currentPageNum cinfo
            bbox = BBox orig (x, y)
            hittestbbox = hltEmbeddedByBBox bbox itms
            selectitms = first unNotHitted hittestbbox
            SelectState thdl = view hoodleModeState uhdl
            newpage = case epage of
              Left pagebbox -> makePageSelectMode pagebbox selectitms
              Right tpage ->
                let currlayer = view (glayers . selectedLayer) tpage
                    newlayer = set gitems (TEitherAlterHitted (Right selectitms)) currlayer
                    npage = set (glayers . selectedLayer) newlayer tpage
                 in npage
            newthdl = set gselSelected (Just (cpn, newpage)) thdl
            ui = view gtkUIManager xstate
        liftIO $ toggleCutCopyDelete ui (isAnyHitted selectitms)
        uhdl' <- liftIO (updatePageAll (SelectState newthdl) uhdl)
        pureUpdateUhdl (const ((hoodleModeState .~ SelectState newthdl) uhdl'))
        commit_
        invalidateAllInBBox Nothing Efficient

-- | prepare for moving selection
startMoveSelect ::
  CanvasId ->
  PageNum ->
  CanvasGeometry ->
  ((Double, Double), UTCTime) ->
  Page SelectMode ->
  MainCoroutine ()
startMoveSelect cid pnum geometry ((x, y), ctime) tpage = do
  cache <- renderCache
  itmimage <- liftIO $ mkItmsNImg cache cid tpage
  tsel <- createTempRender geometry itmimage
  moveSelect cid pnum geometry (x, y) ((x, y), ctime) tsel
  Cairo.surfaceFinish (tempSurfaceSrc tsel)
  Cairo.surfaceFinish (tempSurfaceTgt tsel)
  Cairo.surfaceFinish (imageSurface itmimage)
  invalidateAllInBBox Nothing Efficient

-- |
moveSelect ::
  CanvasId ->
  -- | starting pagenum
  PageNum ->
  CanvasGeometry ->
  (Double, Double) ->
  ((Double, Double), UTCTime) ->
  TempRender ItmsNImg ->
  MainCoroutine ()
moveSelect
  cid
  pnum
  geometry
  orig@(x0, y0)
  (prev, otime)
  tempselection = do
    xst <- get
    let uhdl = view (unitHoodles . currentUnit) xst
    r <- nextevent
    forBoth' unboxBiAct (fsingle r uhdl) (getCanvasInfo cid uhdl)
    where
      fsingle r uhdl cinfo =
        penMoveAndUpInterPage r pnum geometry defact moveact (upact uhdl cinfo)
      defact = moveSelect cid pnum geometry orig (prev, otime) tempselection
      moveact oldpgn pcpair@(newpgn, PageCoord (px, py)) = do
        let (x, y)
              | oldpgn == newpgn = (px, py)
              | otherwise =
                let DeskCoord (xo, yo) = page2Desktop geometry (oldpgn, PageCoord (0, 0))
                    DeskCoord (xn, yn) = page2Desktop geometry pcpair
                 in (xn - xo, yn - yo)
        (willUpdate, (ncoord, ntime)) <- liftIO $ getNewCoordTime (prev, otime) (x, y)
        when willUpdate $ do
          let sfunc = offsetFunc (x - x0, y - y0)
              xform =
                unCvsCoord . desktop2Canvas geometry
                  . page2Desktop geometry
                  . (,) pnum
                  . PageCoord
              (c1, c2) = xform (sfunc (0, 0))
              (a1', a2') = xform (sfunc (1, 0))
              (a1, a2) = (a1' - c1, a2' - c2)
              (b1', b2') = xform (sfunc (0, 1))
              (b1, b2) = (b1' - c1, b2' - c2)
              xformmat = Mat.Matrix a1 a2 b1 b2 c1 c2
          invalidateTempBasePage
            cid
            (tempSurfaceSrc tempselection)
            pnum
            (drawTempSelectImage geometry tempselection xformmat)
        moveSelect cid pnum geometry orig (ncoord, ntime) tempselection
      upact :: UnitHoodle -> CanvasInfo a -> PointerCoord -> MainCoroutine ()
      upact uhdl cinfo pcoord =
        switchActionEnteringDiffPage
          pnum
          geometry
          pcoord
          (return ())
          (chgaction uhdl cinfo)
          (ordaction uhdl cinfo)
      chgaction ::
        UnitHoodle ->
        CanvasInfo a ->
        PageNum ->
        (PageNum, PageCoordinate) ->
        MainCoroutine ()
      chgaction uhdl cinfo oldpgn (newpgn, PageCoord (x, y)) = do
        let hdlmodst@(SelectState thdl) = view hoodleModeState uhdl
            epage = getCurrentPageEitherFromHoodleModeState cinfo hdlmodst
            cvsid = view canvasId cinfo
        (uhdl1, nthdl1, selecteditms) <-
          case epage of
            Right oldtpage -> do
              let itms = getSelectedItms oldtpage
              let oldtpage' = deleteSelected oldtpage
              nthdl <- updateTempHoodleSelectM cvsid thdl oldtpage' (unPageNum oldpgn)
              uhdl' <- liftIO (updatePageAll (SelectState nthdl) uhdl)
              return (uhdl', nthdl, itms)
            Left _ -> error "this is impossible, in moveSelect"
        let maction = do
              page <- M.lookup (unPageNum newpgn) (view gselAll nthdl1)
              let currlayer = getCurrentLayer page
              let olditms = view gitems currlayer
              let newitms = map (changeItemBy (offsetFunc (x - x0, y - y0))) selecteditms
                  alist = olditms :- Hitted newitms :- Empty
                  ntpage = makePageSelectMode page alist
                  coroutineaction = do
                    nthdl2 <- updateTempHoodleSelectM cvsid nthdl1 ntpage (unPageNum newpgn)
                    -- let cibox = view currentCanvasInfo uhdl1
                    --     ncibox = ( runIdentity
                    --             . forBoth unboxBiXform (return . set currentPageNum (unPageNum newpgn)))
                    --               cibox
                    liftIO (updatePageAll (SelectState nthdl2) uhdl1)
              return coroutineaction
        uhdl2 <- maybe (return uhdl1) id maction
        pureUpdateUhdl (const uhdl2)
        commit_
        invalidateAllInBBox Nothing Efficient
      ----
      ordaction uhdl cinfo _pgn (_cpn, PageCoord (x, y)) = do
        let offset = (x - x0, y - y0)
            hdlmodst@(SelectState thdl) = view hoodleModeState uhdl
            epage = getCurrentPageEitherFromHoodleModeState cinfo hdlmodst
            pagenum = view currentPageNum cinfo
            cvsid = view canvasId cinfo
        case epage of
          Right tpage -> do
            let newtpage = changeSelectionByOffset offset tpage
            newthdl <- updateTempHoodleSelectM cvsid thdl newtpage pagenum
            uhdl' <- liftIO (updatePageAll (SelectState newthdl) uhdl)
            pureUpdateUhdl (const uhdl')
            commit_
          Left _ -> error "this is impossible, in moveSelect"
        invalidateAllInBBox Nothing Efficient

-- | prepare for resizing selection
startResizeSelect ::
  -- | doesKeepRatio
  Bool ->
  -- | current selection handle
  Handle ->
  CanvasId ->
  PageNum ->
  CanvasGeometry ->
  BBox ->
  ((Double, Double), UTCTime) ->
  Page SelectMode ->
  MainCoroutine ()
startResizeSelect
  doesKeepRatio
  handle
  cid
  pnum
  geometry
  bbox
  ((x, y), ctime)
  tpage = do
    cache <- renderCache
    itmimage <- liftIO $ mkItmsNImg cache cid tpage
    tsel <- createTempRender geometry itmimage
    resizeSelect
      doesKeepRatio
      handle
      cid
      pnum
      geometry
      bbox
      ((x, y), ctime)
      tsel
    Cairo.surfaceFinish (tempSurfaceSrc tsel)
    Cairo.surfaceFinish (tempSurfaceTgt tsel)
    Cairo.surfaceFinish (imageSurface itmimage)
    invalidateAllInBBox Nothing Efficient

-- |
resizeSelect ::
  -- | doesKeepRatio
  Bool ->
  -- | current selection handle
  Handle ->
  CanvasId ->
  PageNum ->
  CanvasGeometry ->
  BBox ->
  ((Double, Double), UTCTime) ->
  TempRender ItmsNImg ->
  MainCoroutine ()
resizeSelect
  doesKeepRatio
  handle
  cid
  pnum
  geometry
  origbbox
  (prev, otime)
  tempselection = do
    xst <- get
    let uhdl = view (unitHoodles . currentUnit) xst
    r <- nextevent
    forBoth' unboxBiAct (fsingle r uhdl) . getCanvasInfo cid $ uhdl
    where
      fsingle r uhdl cinfo = penMoveAndUpOnly r pnum geometry defact moveact (upact uhdl cinfo)
      defact = resizeSelect doesKeepRatio handle cid pnum geometry origbbox (prev, otime) tempselection
      moveact (_pcoord, (x, y)) = do
        (willUpdate, (ncoord, ntime)) <- liftIO $ getNewCoordTime (prev, otime) (x, y)
        when willUpdate $ do
          let newbbox' = getNewBBoxFromHandlePos handle origbbox (x, y)
              newbbox =
                if doesKeepRatio
                  then
                    let BBox (xo0, yo0) (xo1, yo1) = origbbox
                        BBox (x0, y0) (x1, y1) = newbbox'
                        r = (yo1 - yo0) / (xo1 - xo0)
                     in if  | xo1 == xo0 -> newbbox'
                            | handle == HandleTL -> BBox (x0, y1 + (x0 - x1) * r) (x1, y1)
                            | handle == HandleTR -> BBox (x0, y1 + (x0 - x1) * r) (x1, y1)
                            | handle == HandleBL -> BBox (x0, y0) (x1, y0 + (x1 - x0) * r)
                            | handle == HandleBR -> BBox (x0, y0) (x1, y0 + (x1 - x0) * r)
                            | otherwise -> newbbox'
                  else newbbox'
              sfunc = scaleFromToBBox origbbox newbbox
              xform = unCvsCoord . desktop2Canvas geometry . page2Desktop geometry . (,) pnum . PageCoord
              (c1, c2) = xform (sfunc (0, 0))
              (a1', a2') = xform (sfunc (1, 0))
              (a1, a2) = (a1' - c1, a2' - c2)
              (b1', b2') = xform (sfunc (0, 1))
              (b1, b2) = (b1' - c1, b2' - c2)
              xformmat = Mat.Matrix a1 a2 b1 b2 c1 c2
          invalidateTemp cid (tempSurfaceSrc tempselection) (drawTempSelectImage geometry tempselection xformmat)
        resizeSelect doesKeepRatio handle cid pnum geometry origbbox (ncoord, ntime) tempselection
      upact uhdl cinfo pcoord = do
        let (_, (x, y)) =
              runIdentity $
                skipIfNotInSamePage
                  pnum
                  geometry
                  pcoord
                  (return (pcoord, prev))
                  return
            newbbox' = getNewBBoxFromHandlePos handle origbbox (x, y)
            newbbox =
              if doesKeepRatio
                then
                  let BBox (xo0, yo0) (xo1, yo1) = origbbox
                      BBox (x0, y0) (x1, y1) = newbbox'
                      r = (yo1 - yo0) / (xo1 - xo0)
                   in if  | xo1 == xo0 || yo1 == yo0 -> newbbox'
                          | handle == HandleTL -> BBox (x0, y1 + (x0 - x1) * r) (x1, y1)
                          | handle == HandleTR -> BBox (x0, y1 + (x0 - x1) * r) (x1, y1)
                          | handle == HandleBL -> BBox (x0, y0) (x1, y0 + (x1 - x0) * r)
                          | handle == HandleBR -> BBox (x0, y0) (x1, y0 + (x1 - x0) * r)
                          | otherwise -> newbbox'
                else newbbox'
            hdlmodst@(SelectState thdl) = view hoodleModeState uhdl
            epage = getCurrentPageEitherFromHoodleModeState cinfo hdlmodst
            pagenum = view currentPageNum cinfo
            cvsid = view canvasId cinfo
        case epage of
          Right tpage -> do
            let sfunc = scaleFromToBBox origbbox newbbox
                newtpage = changeSelectionBy sfunc tpage
            newthdl <- updateTempHoodleSelectM cvsid thdl newtpage pagenum
            uhdl' <- liftIO (updatePageAll (SelectState newthdl) uhdl)
            pureUpdateUhdl (const uhdl')
            commit_
          Left _ -> error "this is impossible, in resizeSelect"
        invalidateAllInBBox Nothing Efficient
        return ()

-- |
selectPenColorChanged :: PenColor -> MainCoroutine ()
selectPenColorChanged pcolor = do
  uhdl <- view (unitHoodles . currentUnit) <$> get
  let cid = getCurrentCanvasId uhdl
      SelectState thdl = view hoodleModeState uhdl
      Just (n, tpage) = view gselSelected thdl
      slayer = view (glayers . selectedLayer) tpage
  case unTEitherAlterHitted . view gitems $ slayer of
    Left _ -> return ()
    Right alist -> do
      let alist' = second (Hitted . map (changeItemStrokeColor pcolor) . unHitted) alist
          newlayer = Right alist'
          newpage = (glayers . selectedLayer .~ (GLayer (slayer ^. gbuffer) (TEitherAlterHitted newlayer))) tpage
      newthdl <- updateTempHoodleSelectM cid thdl newpage n
      uhdl' <- liftIO (updatePageAll (SelectState newthdl) uhdl)
      pureUpdateUhdl (const uhdl')
      commit_
      invalidateAllInBBox Nothing Efficient

-- |
selectPenWidthChanged :: Double -> MainCoroutine ()
selectPenWidthChanged pwidth = do
  xst <- get
  let uhdl = view (unitHoodles . currentUnit) xst
      cid = getCurrentCanvasId uhdl
      SelectState thdl = view hoodleModeState uhdl
      Just (n, tpage) = view gselSelected thdl
      slayer = view (glayers . selectedLayer) tpage
  case (unTEitherAlterHitted . view gitems) slayer of
    Left _ -> return ()
    Right alist -> do
      let alist' = second (Hitted . map (changeItemStrokeWidth pwidth) . unHitted) alist
          newlayer = Right alist'
          newpage = set (glayers . selectedLayer) (GLayer (view gbuffer slayer) (TEitherAlterHitted newlayer)) tpage
      newthdl <- updateTempHoodleSelectM cid thdl newpage n
      uhdl' <- liftIO (updatePageAll (SelectState newthdl) uhdl)
      pureUpdateUhdl (const uhdl')
      commit_
      invalidateAllInBBox Nothing Efficient

-- | main mouse pointer click entrance in lasso selection mode.
--   choose either starting new rectangular selection or move previously
--   selected selection.
selectLassoStart :: PenButton -> CanvasId -> PointerCoord -> MainCoroutine ()
selectLassoStart p cid coord = commonSelectStart SelectLassoWork p cid coord >> return ()

-- |
newSelectLasso ::
  CanvasInfo a ->
  PageNum ->
  CanvasGeometry ->
  [RItem] ->
  (Double, Double) ->
  ((Double, Double), UTCTime) ->
  Seq (Double, Double) ->
  TempSelection ->
  MainCoroutine ()
newSelectLasso cvsInfo pnum geometry itms orig (prev, otime) lasso tsel = nextevent >>= flip fsingle cvsInfo
  where
    fsingle r cinfo = penMoveAndUpOnly r pnum geometry defact (moveact cinfo) (upact cinfo)
    defact =
      newSelectLasso
        cvsInfo
        pnum
        geometry
        itms
        orig
        (prev, otime)
        lasso
        tsel
    moveact cinfo (_pcoord, (x, y)) = do
      let nlasso = lasso |> (x, y)
      (willUpdate, (ncoord, ntime)) <- liftIO $ getNewCoordTime (prev, otime) (x, y)
      when willUpdate $ invalidateTemp (view canvasId cinfo) (tempSurfaceSrc tsel) (renderLasso geometry nlasso)
      newSelectLasso cinfo pnum geometry itms orig (ncoord, ntime) nlasso tsel
    upact cinfo pcoord = do
      uhdl <- view (unitHoodles . currentUnit) <$> get
      let (_, (x, y)) = runIdentity $ skipIfNotInSamePage pnum geometry pcoord (return (pcoord, prev)) return
          nlasso = lasso |> (x, y)
          hdlmodst = view hoodleModeState uhdl
          epage = getCurrentPageEitherFromHoodleModeState cinfo hdlmodst
          cpn = view currentPageNum cinfo
          hittestlasso1 = hltFilteredBy (hitLassoItem (nlasso |> orig)) itms
          selectitms1 = first unNotHitted hittestlasso1
          selecteditms1 = (concatMap unHitted . getB) selectitms1
          hittestlasso2 = takeLastFromHitted . flip hltFilteredBy itms $
            \itm ->
              (not . isStrkInRItem) itm
                && isPointInBBox (getBBox itm) (x, y)
          selectitms2 = first unNotHitted hittestlasso2
          selectitms
            | (not . null) selecteditms1 = selectitms1
            | otherwise = selectitms2
          SelectState thdl = view hoodleModeState uhdl
          newpage = case epage of
            Left pagebbox ->
              let currlayer = getCurrentLayer pagebbox
                  newlayer = GLayer (view gbuffer currlayer) (TEitherAlterHitted (Right selectitms))
                  tpg = mkHPage pagebbox
                  npg = set (glayers . selectedLayer) newlayer tpg
               in npg
            Right tpage ->
              let currlayer = view (glayers . selectedLayer) tpage
                  newlayer = GLayer (view gbuffer currlayer) (TEitherAlterHitted (Right selectitms))
                  npage = set (glayers . selectedLayer) newlayer tpage
               in npage
          newthdl = set gselSelected (Just (cpn, newpage)) thdl
      ui <- view gtkUIManager <$> get
      liftIO $ toggleCutCopyDelete ui (isAnyHitted selectitms)
      uhdl' <- liftIO (updatePageAll (SelectState newthdl) uhdl)
      pureUpdateUhdl (const ((hoodleModeState .~ SelectState newthdl) uhdl'))
      commit_
      invalidateAllInBBox Nothing Efficient
