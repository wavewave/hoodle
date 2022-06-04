{-# LANGUAGE CPP #-}

module Hoodle.Coroutine.VerticalSpace where

import Control.Category ((.))
import Control.Lens (at, set, view, (.~))
import Control.Monad ((>=>))
import Control.Monad.State (gets)
import Control.Monad.Trans (liftIO)
import Data.Bifunctor (second)
import Data.Foldable (concat, concatMap, mapM_)
import Data.Hoodle.BBox
  ( BBox (..),
    ULMaybe (Middle),
    UnionBBox (..),
    getBBox,
    toMaybe,
  )
import Data.Hoodle.Generic
  ( gdimension,
    gitems,
    glayers,
    gpages,
  )
import Data.Hoodle.Simple (Dimension (..))
import Data.Hoodle.Zipper (SeqZipper, toSeq)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Graphics.Hoodle.Render (renderRItem)
import Graphics.Hoodle.Render.Type
  ( RItem (..),
    SurfaceID,
    emptyRLayer,
    issueSurfaceID,
  )
import Graphics.Hoodle.Render.Type.HitTest
  ( Hitted (..),
    NotHitted (..),
    RItemHitted,
    getA,
    getB,
    interleave,
  )
import Graphics.Hoodle.Render.Util.HitTest (hltFilteredBy)
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.UI.Gtk as Gtk
import Hoodle.Accessor
  ( getCurrentPageCurr,
    pureUpdateUhdl,
    renderCache,
    updateUhdl,
  )
import Hoodle.Coroutine.Commit (commit_)
import Hoodle.Coroutine.Draw
  ( invalidateAll,
    nextevent,
  )
import Hoodle.Coroutine.Page (addNewPageInHoodle, canvasZoomUpdateAll)
import Hoodle.Coroutine.Pen
  ( commonPenStart,
    penMoveAndUpOnly,
    processWithDefTimeInterval,
  )
import Hoodle.Device (PointerCoord)
import Hoodle.ModelAction.Page (updatePageAll)
import Hoodle.ModelAction.Select.Transform (changeItemBy)
import Hoodle.Type.Alias (EditMode, Hoodle, Page)
import Hoodle.Type.Canvas
  ( CanvasId,
    CanvasInfo (..),
    drawArea,
    forBoth',
    unboxBiAct,
  )
import Hoodle.Type.Coroutine (MainCoroutine)
import Hoodle.Type.Enum
  ( AddDirection (PageAfter),
    BackgroundStyle,
    VerticalSpaceMode (GoingDown, GoingUp, OverPage),
  )
import Hoodle.Type.Event (UserEvent)
import Hoodle.Type.HoodleState
  ( HoodleModeState (SelectState, ViewAppendState),
    UnitHoodle,
    backgroundStyle,
    currentUnit,
    getCanvasInfo,
    getHoodle,
    hoodleModeState,
    unitHoodles,
  )
import Hoodle.Type.PageArrangement
  ( CanvasCoordinate (..),
    PageCoordinate (..),
    PageNum (..),
  )
import Hoodle.Type.Predefined (predefinedLassoWidth)
import Hoodle.Util (msgShout)
import Hoodle.View.Coordinate
  ( CanvasGeometry,
    desktop2Canvas,
    desktop2Page,
    device2Desktop,
    page2Desktop,
  )
import Hoodle.View.Draw (cairoXform4PageCoordinate, canvas2DesktopRatio, canvasImageSurface, mkXform4Page)
--
import Prelude hiding (concat, concatMap, id, mapM_, (.))

-- |
splitPageByHLine ::
  Double ->
  Page EditMode ->
  ([RItem], Page EditMode, SeqZipper RItemHitted)
splitPageByHLine y pg = (hitted, set glayers unhitted pg, hltedLayers)
  where
    alllyrs = view glayers pg
    findHittedItmsInALyr = hltFilteredBy (bboxabove . getBBox) . view gitems
    hltedLayers = fmap findHittedItmsInALyr alllyrs
    unhitted =
      fmap
        ( \lyr ->
            (\x -> set gitems x lyr) . concatMap unNotHitted
              . getA
              . findHittedItmsInALyr
              $ lyr
        )
        alllyrs
    hitted = concatMap (concatMap unHitted . getB . findHittedItmsInALyr) . toSeq $ alllyrs
    bboxabove (BBox (_, y0) _) = y0 > y

-- |
verticalSpaceStart :: CanvasId -> PointerCoord -> MainCoroutine ()
verticalSpaceStart cid = commonPenStart verticalSpaceAction cid >=> const (return ())
  where
    verticalSpaceAction _cinfo pnum@(PageNum n) geometry (x, y) _ = do
      hdl <- gets (getHoodle . view (unitHoodles . currentUnit))
      cache <- renderCache
      cpg <- getCurrentPageCurr
      let (itms, npg, hltedLayers) = splitPageByHLine y cpg
          nhdl = set (gpages . at n) (Just npg) hdl
          mbbx = (toMaybe . mconcat . fmap (Union . Middle . getBBox)) itms
      case mbbx of
        Nothing -> return ()
        Just bbx -> do
          (sfcbkg, Dim w h) <- liftIO $ canvasImageSurface cache cid Nothing geometry nhdl
          sfcitm <-
            liftIO $
              Cairo.createImageSurface
                Cairo.FormatARGB32
                (floor w)
                (floor h)
          sfctot <-
            liftIO $
              Cairo.createImageSurface
                Cairo.FormatARGB32
                (floor w)
                (floor h)
          liftIO $
            Cairo.renderWith sfcitm $ do
              Cairo.identityMatrix
              cairoXform4PageCoordinate (mkXform4Page geometry pnum)
              mapM_ (renderRItem cache cid) itms
          ctime <- liftIO getCurrentTime
          verticalSpaceProcess
            cid
            geometry
            (bbx, hltedLayers, pnum, cpg)
            (x, y)
            (sfcbkg, sfcitm, sfctot)
            ctime
          liftIO $ mapM_ Cairo.surfaceFinish [sfcbkg, sfcitm, sfctot]

-- |
addNewPageAndMoveBelow ::
  (PageNum, SeqZipper RItemHitted, BBox) ->
  MainCoroutine ()
addNewPageAndMoveBelow (pnum, hltedLyrs, bbx) = do
  bsty <- gets (view backgroundStyle)
  updateUhdl (npgact bsty) >> commit_ >> canvasZoomUpdateAll >> invalidateAll
  where
    npgact :: BackgroundStyle -> UnitHoodle -> MainCoroutine UnitHoodle
    npgact bsty uhdl = do
      case view hoodleModeState uhdl of
        ViewAppendState hdl -> do
          hdl' <- addNewPageInHoodle Nothing bsty PageAfter hdl (unPageNum pnum)
          sfcid <- issueSurfaceID
          sfcid2 <- issueSurfaceID
          let nhdlmodst = ViewAppendState (moveBelowToNewPage (sfcid, sfcid2) (pnum, hltedLyrs, bbx) hdl')
          liftIO . updatePageAll nhdlmodst . (hoodleModeState .~ nhdlmodst) $ uhdl
        SelectState _ -> do
          msgShout "addNewPageAndMoveBelow: not implemented yet"
          return uhdl

-- |
moveBelowToNewPage ::
  -- | sfcid: old page, sfcid2: new page
  (SurfaceID, SurfaceID) ->
  (PageNum, SeqZipper RItemHitted, BBox) ->
  Hoodle EditMode ->
  Hoodle EditMode
moveBelowToNewPage (sfcid, sfcid2) (PageNum n, hltedLayers, BBox (_, y0) _) hdl =
  let mpg = view (gpages . at n) hdl
      mpg2 = view (gpages . at (n + 1)) hdl
   in case (,) <$> mpg <*> mpg2 of
        Nothing -> hdl
        Just (pg, pg2) ->
          let nhlyrs =
                -- 10 is just a predefined number
                fmap (second (fmap (changeItemBy (\(x', y') -> (x', y' + 10 - y0))))) hltedLayers
              nlyrs =
                fmap
                  ( (\x -> set gitems x (emptyRLayer sfcid))
                      . concatMap unNotHitted
                      . getA
                  )
                  nhlyrs
              npg = set glayers nlyrs pg
              nnlyrs =
                fmap
                  ( (\x -> set gitems x (emptyRLayer sfcid2))
                      . concatMap unHitted
                      . getB
                  )
                  nhlyrs
              npg2 = set glayers nnlyrs pg2
              nhdl =
                ( set (gpages . at (n + 1)) (Just npg2)
                    . set (gpages . at n) (Just npg)
                )
                  hdl
           in nhdl

-- |
verticalSpaceProcess ::
  CanvasId ->
  CanvasGeometry ->
  (BBox, SeqZipper RItemHitted, PageNum, Page EditMode) ->
  (Double, Double) ->
  -- | (background, item, total)
  (Cairo.Surface, Cairo.Surface, Cairo.Surface) ->
  UTCTime ->
  MainCoroutine ()
verticalSpaceProcess
  cid
  geometry
  pinfo@(bbx, hltedLayers, pnum@(PageNum n), pg)
  (x0, y0)
  sfcs@(sfcbkg, sfcitm, sfctot)
  otime = do
    r <- nextevent
    uhdl <- gets (view (unitHoodles . currentUnit))
    forBoth' unboxBiAct (f r) . getCanvasInfo cid $ uhdl
    where
      Dim w h = view gdimension pg
      CvsCoord (_, y0_cvs) = (desktop2Canvas geometry . page2Desktop geometry) (pnum, PageCoord (x0, y0))
      -------------------------------------------------------------
      f :: UserEvent -> CanvasInfo a -> MainCoroutine ()
      f r cvsInfo = penMoveAndUpOnly r pnum geometry defact (moveact cvsInfo) upact
      defact = verticalSpaceProcess cid geometry pinfo (x0, y0) sfcs otime
      upact pcoord = do
        let mpgcoord = (desktop2Page geometry . device2Desktop geometry) pcoord
        case mpgcoord of
          Nothing -> invalidateAll
          Just (cpn, PageCoord (_, y)) ->
            if cpn /= pnum
              then invalidateAll
              else do
                -- add space  within this page
                let BBox _ (_, by1) = bbx
                if by1 + y - y0 < h
                  then do
                    sfcid <- issueSurfaceID
                    pureUpdateUhdl $ \uhdl -> do
                      let hdl = getHoodle uhdl
                          nhlyrs = fmap (second (fmap (changeItemBy (\(x', y') -> (x', y' + y - y0))))) hltedLayers
                          nlyrs =
                            fmap
                              ( (\is -> set gitems is (emptyRLayer sfcid))
                                  . concat
                                  . interleave unNotHitted unHitted
                              )
                              nhlyrs
                          npg = set glayers nlyrs pg
                          nhdl = set (gpages . at n) (Just npg) hdl
                          nhdlmodst = ViewAppendState nhdl
                          nuhdl = (hoodleModeState .~ nhdlmodst) uhdl
                       in nuhdl
                    commit_
                    canvasZoomUpdateAll
                    invalidateAll
                  else addNewPageAndMoveBelow (pnum, hltedLayers, bbx)
      -------------------------------------------------------------
      moveact cvsInfo (_, (x, y)) =
        processWithDefTimeInterval
          (verticalSpaceProcess cid geometry pinfo (x0, y0) sfcs)
          ( \ctime -> do
              let CvsCoord (_, y_cvs) =
                    (desktop2Canvas geometry . page2Desktop geometry) (pnum, PageCoord (x, y))
                  BBox _ (_, by1) = bbx
                  mode
                    | by1 + y - y0 > h = OverPage
                    | y > y0 = GoingDown
                    | otherwise = GoingUp
                  z = canvas2DesktopRatio geometry
                  drawguide = do
                    Cairo.identityMatrix
                    cairoXform4PageCoordinate (mkXform4Page geometry pnum)
                    Cairo.setLineWidth (predefinedLassoWidth * z)
                    case mode of
                      GoingUp -> Cairo.setSourceRGBA 0.1 0.8 0.1 0.4
                      GoingDown -> Cairo.setSourceRGBA 0.1 0.1 0.8 0.4
                      OverPage -> Cairo.setSourceRGBA 0.8 0.1 0.1 0.4
                    Cairo.moveTo 0 y0
                    Cairo.lineTo w y0
                    Cairo.stroke
                    Cairo.moveTo 0 y
                    Cairo.lineTo w y
                    Cairo.stroke
                    case mode of
                      GoingUp ->
                        Cairo.setSourceRGBA 0.1 0.8 0.1 0.2
                          >> Cairo.rectangle 0 y w (y0 - y)
                      GoingDown ->
                        Cairo.setSourceRGBA 0.1 0.1 0.8 0.2
                          >> Cairo.rectangle 0 y0 w (y - y0)
                      OverPage ->
                        Cairo.setSourceRGBA 0.8 0.1 0.1 0.2
                          >> Cairo.rectangle 0 y0 w (y - y0)
                    Cairo.fill
              liftIO $
                Cairo.renderWith sfctot $ do
                  Cairo.setSourceSurface sfcbkg 0 0
                  Cairo.setOperator Cairo.OperatorSource
                  Cairo.paint
                  Cairo.setSourceSurface sfcitm 0 (y_cvs - y0_cvs)
                  Cairo.setOperator Cairo.OperatorOver
                  Cairo.paint
                  drawguide
              let canvas = view drawArea cvsInfo
              Just win <- liftIO $ Gtk.widgetGetWindow canvas
              liftIO $
                Gtk.renderWithDrawWindow win $ do
                  Cairo.setSourceSurface sfctot 0 0
                  Cairo.setOperator Cairo.OperatorSource
                  Cairo.paint
              verticalSpaceProcess cid geometry pinfo (x0, y0) sfcs ctime
          )
          otime
