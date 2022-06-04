{-# LANGUAGE GADTs #-}

module Hoodle.Coroutine.Mode where

import Control.Lens (set, view, (.~))
import Control.Monad.State (liftIO)
import Data.Hoodle.BBox (bbox_upperleft)
import Data.Hoodle.Generic (gdimension)
import Data.Hoodle.Select
  ( gHoodle2GSelect,
    gSelect2GHoodle,
    gselAll,
    gselSelected,
  )
import qualified Data.IntMap as M
import Graphics.Hoodle.Render (updatePageBuf)
import Graphics.Hoodle.Render.Type (hPage2RPage)
import qualified Graphics.UI.Gtk as Gtk (adjustmentGetValue)
import Hoodle.Accessor
  ( getCurrentPageCurr,
    getGeometry4CurrCvs,
    updateUhdl,
    updateXState,
  )
import Hoodle.Coroutine.Draw
  ( callRenderer_,
    invalidateAll,
    invalidateAllInBBox,
  )
import Hoodle.Coroutine.Scroll (adjustScrollbarWithGeometryCurrent)
import Hoodle.GUI.Reflect
  ( reflectPenColorUI,
    reflectPenModeUI,
    reflectPenWidthUI,
  )
import Hoodle.Type.Alias (EditMode, Hoodle, SelectMode)
import Hoodle.Type.Canvas
  ( CanvasInfo (..),
    CanvasInfoBox (CanvasContPage, CanvasSinglePage),
    ViewInfo (..),
    adjustments,
    canvasId,
    canvasWidgets,
    currentPageNum,
    drawArea,
    horizAdjConnId,
    horizAdjustment,
    mDrawSurface,
    notifiedItem,
    pageArrangement,
    scrolledWindow,
    unboxBiAct,
    vertAdjConnId,
    vertAdjustment,
    viewInfo,
    zoomMode,
  )
import Hoodle.Type.Coroutine (MainCoroutine)
import Hoodle.Type.Enum (DrawFlag (Efficient))
import Hoodle.Type.Event
  ( UserEvent
      ( ToContSinglePage,
        ToSelectMode,
        ToSinglePage,
        ToViewAppendMode
      ),
  )
import Hoodle.Type.HoodleState
  ( HoodleModeState (SelectState, ViewAppendState),
    HoodleState,
    UnitHoodle,
    currentCanvasInfo,
    currentUnit,
    getCurrentCanvasId,
    getHoodle,
    hoodleModeState,
    hoodleModeStateEither,
    unitHoodles,
  )
import Hoodle.Type.PageArrangement
  ( DesktopCoordinate (..),
    PageCoordinate (..),
    PageDimension (..),
    PageNum (..),
    ViewPortBBox (..),
    makeContinuousArrangement,
    makeSingleArrangement,
    viewPortBBox,
  )
import Hoodle.View.Coordinate
  ( canvasDim,
    desktop2Page,
    makeCanvasGeometry,
    page2Desktop,
  )
--
import Prelude hiding (mapM, mapM_)

modeChange :: UserEvent -> MainCoroutine ()
modeChange command = do
  case command of
    ToViewAppendMode -> updateXState select2edit >> invalidateAll
    ToSelectMode -> updateXState edit2select >> invalidateAllInBBox Nothing Efficient
    _ -> return ()
  reflectPenModeUI
  reflectPenColorUI
  reflectPenWidthUI
  where
    select2edit xst =
      either (noaction xst) (whenselect xst) . hoodleModeStateEither
        . view (unitHoodles . currentUnit . hoodleModeState)
        $ xst
    edit2select xst =
      either (whenedit xst) (noaction xst) . hoodleModeStateEither
        . view (unitHoodles . currentUnit . hoodleModeState)
        $ xst
    noaction :: HoodleState -> a -> MainCoroutine HoodleState
    noaction xst = const (return xst)
    whenselect :: HoodleState -> Hoodle SelectMode -> MainCoroutine HoodleState
    whenselect xst thdl = do
      let pages = view gselAll thdl
          mselect = view gselSelected thdl
          cid = getCurrentCanvasId (view (unitHoodles . currentUnit) xst)
      npages <-
        maybe
          (return pages)
          ( \(spgn, spage) -> do
              let npage = hPage2RPage spage
              callRenderer_ $ updatePageBuf cid npage
              return $ M.adjust (const npage) spgn pages
          )
          mselect
      let nthdl = set gselAll npages . set gselSelected Nothing $ thdl
      return $ (unitHoodles . currentUnit . hoodleModeState .~ ViewAppendState (gSelect2GHoodle nthdl)) xst
    whenedit :: HoodleState -> Hoodle EditMode -> MainCoroutine HoodleState
    whenedit xst hdl =
      return $ (unitHoodles . currentUnit . hoodleModeState .~ SelectState (gHoodle2GSelect hdl)) xst

-- |
viewModeChange :: UserEvent -> MainCoroutine ()
viewModeChange command = do
  case command of
    ToSinglePage -> updateUhdl cont2single >> invalidateAll
    ToContSinglePage -> updateUhdl single2cont >> invalidateAll
    _ -> return ()
  adjustScrollbarWithGeometryCurrent
  where
    cont2single :: UnitHoodle -> MainCoroutine UnitHoodle
    cont2single uhdl = unboxBiAct (const (return uhdl)) (cont2SingPage uhdl) . view currentCanvasInfo $ uhdl
    single2cont :: UnitHoodle -> MainCoroutine UnitHoodle
    single2cont uhdl = unboxBiAct (sing2ContPage uhdl) (const (return uhdl)) . view currentCanvasInfo $ uhdl

cont2SingPage :: UnitHoodle -> CanvasInfo a -> MainCoroutine UnitHoodle
cont2SingPage uhdl cinfo = do
  geometry <- liftIO $ getGeometry4CurrCvs uhdl
  cdim <- liftIO $ return . canvasDim $ geometry
  page <- getCurrentPageCurr
  let zmode = view (viewInfo . zoomMode) cinfo
      canvas = view drawArea cinfo
      cpn = PageNum . view currentPageNum $ cinfo
      pdim = PageDimension (view gdimension page)
      ViewPortBBox bbox = view (viewInfo . pageArrangement . viewPortBBox) cinfo
      (x0, y0) = bbox_upperleft bbox
      (xpos, ypos) = maybe (0, 0) (unPageCoord . snd) $ desktop2Page geometry (DeskCoord (x0, y0))
  let arr = makeSingleArrangement zmode pdim cdim (xpos, ypos)
  let nvinfo = ViewInfo (view zoomMode (view viewInfo cinfo)) arr
      ncinfo =
        CanvasInfo
          (view canvasId cinfo)
          canvas
          (view mDrawSurface cinfo)
          (view scrolledWindow cinfo)
          nvinfo
          (unPageNum cpn)
          (view horizAdjustment cinfo)
          (view vertAdjustment cinfo)
          (view horizAdjConnId cinfo)
          (view vertAdjConnId cinfo)
          (view canvasWidgets cinfo)
          (view notifiedItem cinfo)
  return $ (currentCanvasInfo .~ CanvasSinglePage ncinfo) uhdl

sing2ContPage :: UnitHoodle -> CanvasInfo a -> MainCoroutine UnitHoodle
sing2ContPage uhdl cinfo = do
  cdim <-
    liftIO $
      canvasDim <$> getGeometry4CurrCvs uhdl
  let zmode = view (viewInfo . zoomMode) cinfo
      canvas = view drawArea cinfo
      cpn = PageNum . view currentPageNum $ cinfo
      (hadj, vadj) = view adjustments cinfo
  (xpos, ypos) <- liftIO $ (,) <$> Gtk.adjustmentGetValue hadj <*> Gtk.adjustmentGetValue vadj
  let arr =
        makeContinuousArrangement
          zmode
          cdim
          (getHoodle uhdl)
          (cpn, PageCoord (xpos, ypos))
  geometry <- liftIO $ makeCanvasGeometry cpn arr canvas
  let DeskCoord (nxpos, nypos) = page2Desktop geometry (cpn, PageCoord (xpos, ypos))
  let vinfo = view viewInfo cinfo
      nvinfo = ViewInfo (view zoomMode vinfo) arr
      ncinfotemp =
        CanvasInfo
          (view canvasId cinfo)
          (view drawArea cinfo)
          (view mDrawSurface cinfo)
          (view scrolledWindow cinfo)
          nvinfo
          (view currentPageNum cinfo)
          hadj
          vadj
          (view horizAdjConnId cinfo)
          (view vertAdjConnId cinfo)
          (view canvasWidgets cinfo)
          (view notifiedItem cinfo)
      ncpn = maybe cpn fst $ desktop2Page geometry (DeskCoord (nxpos, nypos))
      ncinfo = (currentPageNum .~ unPageNum ncpn) ncinfotemp
  return $ (currentCanvasInfo .~ CanvasContPage ncinfo) uhdl
