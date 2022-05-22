{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : Hoodle.ModelAction.Page
-- Copyright   : (c) 2011-2016 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
module Hoodle.ModelAction.Page where

import Control.Applicative
import Control.Lens (set, view)
import Control.Monad (liftM)
import Data.Functor.Identity (Identity (..))
-- from hoodle-platform
import Data.Hoodle.Generic
import Data.Hoodle.Select
import qualified Data.IntMap as M
import Data.Traversable (mapM)
import qualified Graphics.UI.Gtk as Gtk (adjustmentGetValue)
-- from this package

import Hoodle.Type.Alias
import Hoodle.Type.Canvas
import Hoodle.Type.Enum
import Hoodle.Type.HoodleState
import Hoodle.Type.PageArrangement
import Hoodle.Type.Predefined
import Hoodle.Util
import Hoodle.View.Coordinate
--
import Prelude hiding (mapM)

-- |
getPageMap :: HoodleModeState -> M.IntMap (Page EditMode)
getPageMap = either (view gpages) (view gselAll) . hoodleModeStateEither

-- |
setPageMap :: M.IntMap (Page EditMode) -> HoodleModeState -> HoodleModeState
setPageMap nmap =
  either
    (ViewAppendState . set gpages nmap)
    (SelectState . set gselSelected Nothing . set gselAll nmap)
    . hoodleModeStateEither

-- |
updatePageAll :: HoodleModeState -> UnitHoodle -> IO UnitHoodle
updatePageAll hdlmodst uhdl = do
  let cmap = view cvsInfoMap uhdl
  cmap' <- mapM (updatePage hdlmodst . adjustPage hdlmodst) cmap
  return $
    maybe uhdl id
      . setCanvasInfoMap cmap'
      . set hoodleModeState hdlmodst
      $ uhdl

-- |
adjustPage :: HoodleModeState -> CanvasInfoBox -> CanvasInfoBox
adjustPage hdlmodst = runIdentity . forBoth unboxBiXform (return . fsingle)
  where
    fsingle :: CanvasInfo a -> CanvasInfo a
    fsingle cinfo =
      let cpn = view currentPageNum cinfo
          pagemap = getPageMap hdlmodst
       in adjustwork cpn pagemap
      where
        adjustwork cpn pagemap =
          if M.notMember cpn pagemap
            then
              let (minp, _) = M.findMin pagemap
                  (maxp, _) = M.findMax pagemap
               in if cpn > maxp
                    then set currentPageNum maxp cinfo
                    else set currentPageNum minp cinfo
            else cinfo

-- |
getPageFromGHoodleMap :: Int -> GHoodle M.IntMap a -> a
getPageFromGHoodleMap pagenum =
  maybeError' ("getPageFromGHoodleMap " ++ show pagenum) . M.lookup pagenum . view gpages

-- |
updateCvsInfoFrmHoodle :: Hoodle EditMode -> CanvasInfoBox -> IO CanvasInfoBox
updateCvsInfoFrmHoodle hdl (CanvasSinglePage cinfo) = do
  let pagenum = view currentPageNum cinfo
      oarr = view (viewInfo . pageArrangement) cinfo
      canvas = view drawArea cinfo
      zmode = view (viewInfo . zoomMode) cinfo
  geometry <- makeCanvasGeometry (PageNum pagenum) oarr canvas
  let cdim = canvasDim geometry
      pg = getPageFromGHoodleMap pagenum hdl
      pdim = PageDimension $ view gdimension pg
      (hadj, vadj) = view adjustments cinfo
  (xpos, ypos) <- (,) <$> Gtk.adjustmentGetValue hadj <*> Gtk.adjustmentGetValue vadj
  let arr = makeSingleArrangement zmode pdim cdim (xpos, ypos)
      vinfo = view viewInfo cinfo
      nvinfo = xfrmViewInfo (const arr) vinfo
  return
    . CanvasSinglePage
    . set currentPageNum pagenum
    . xfrmCvsInfo (const nvinfo)
    $ cinfo
updateCvsInfoFrmHoodle hdl (CanvasContPage cinfo) = do
  let pagenum = view currentPageNum cinfo
      oarr = view (viewInfo . pageArrangement) cinfo
      canvas = view drawArea cinfo
      zmode = view (viewInfo . zoomMode) cinfo
      (hadj, vadj) = view adjustments cinfo
  (xdesk, ydesk) <-
    (,) <$> Gtk.adjustmentGetValue hadj
      <*> Gtk.adjustmentGetValue vadj
  geometry <- makeCanvasGeometry (PageNum pagenum) oarr canvas
  case desktop2Page geometry (DeskCoord (xdesk, ydesk)) of
    Nothing -> return (CanvasContPage cinfo)
    Just ulcoord -> do
      let cdim = canvasDim geometry
          arr = makeContinuousArrangement zmode cdim hdl ulcoord
      let vinfo = view viewInfo cinfo
          nvinfo = xfrmViewInfo (const arr) vinfo
      return
        . CanvasContPage
        . set currentPageNum pagenum
        . xfrmCvsInfo (const nvinfo)
        $ cinfo

-- |
updatePage :: HoodleModeState -> CanvasInfoBox -> IO CanvasInfoBox
updatePage (ViewAppendState hdl) c = updateCvsInfoFrmHoodle hdl c
updatePage (SelectState thdl) c = do
  let hdl = gSelect2GHoodle thdl
  updateCvsInfoFrmHoodle hdl c

-- |
setPage :: UnitHoodle -> PageNum -> CanvasId -> IO CanvasInfoBox
setPage uhdl pnum cid = do
  let cinfobox = getCanvasInfo cid uhdl
  unboxBiAct
    (liftM CanvasSinglePage . setPageSingle uhdl pnum)
    (liftM CanvasContPage . setPageCont uhdl pnum)
    cinfobox

-- | setPageSingle : in Single Page mode
setPageSingle ::
  UnitHoodle ->
  PageNum ->
  CanvasInfo 'SinglePage ->
  IO (CanvasInfo 'SinglePage)
setPageSingle uhdl pnum cinfo = do
  let hdl = getHoodle uhdl
  geometry <- getCvsGeomFrmCvsInfo cinfo
  let cdim = canvasDim geometry
  let pg = getPageFromGHoodleMap (unPageNum pnum) hdl
      pdim = PageDimension (view gdimension pg)
      zmode = view (viewInfo . zoomMode) cinfo
      arr = makeSingleArrangement zmode pdim cdim (0, 0)
  return $
    set currentPageNum (unPageNum pnum)
      . set (viewInfo . pageArrangement) arr
      $ cinfo

-- | setPageCont : in Continuous Page mode
setPageCont ::
  UnitHoodle ->
  PageNum ->
  CanvasInfo 'ContinuousPage ->
  IO (CanvasInfo 'ContinuousPage)
setPageCont uhdl pnum cinfo = do
  let hdl = getHoodle uhdl
  geometry <- getCvsGeomFrmCvsInfo cinfo
  let cdim = canvasDim geometry
      zmode = view (viewInfo . zoomMode) cinfo
      arr = makeContinuousArrangement zmode cdim hdl (pnum, PageCoord (0, 0))
  return $
    set currentPageNum (unPageNum pnum)
      . set (viewInfo . pageArrangement) arr
      $ cinfo

-- | need to be refactored into zoomRatioFrmRelToCurr (rename zoomRatioRelPredefined)
relZoomRatio :: CanvasGeometry -> ZoomModeRel -> Double
relZoomRatio geometry rzmode =
  let CvsCoord (cx0, _cy0) = desktop2Canvas geometry (DeskCoord (0, 0))
      CvsCoord (cx1, _cy1) = desktop2Canvas geometry (DeskCoord (1, 1))
      scalefactor = case rzmode of
        ZoomIn -> predefinedZoomStepFactor
        ZoomOut -> 1.0 / predefinedZoomStepFactor
   in (cx1 - cx0) * scalefactor

-- |
zoomRatioFrmRelToCurr :: CanvasGeometry -> Double -> Double
zoomRatioFrmRelToCurr geometry z =
  let CvsCoord (cx0, _cy0) = desktop2Canvas geometry (DeskCoord (0, 0))
      CvsCoord (cx1, _cy1) = desktop2Canvas geometry (DeskCoord (1, 1))
   in (cx1 - cx0) * z
