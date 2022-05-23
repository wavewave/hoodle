{-# LANGUAGE CPP #-}

-- NOTE: comment out this module for now.

module Graphics.Hoodle.Render.Debug
  (
  )
where

{-
    -- * dummy rendering
    renderRBkg_Dummy,

    -- * nopdf
    renderRBkg_NoPDF,

    -- * render only bbox (for debug purpose)
    renderStrkBBx_BBoxOnly,
    renderImgBBx_BBoxOnly,
    renderRItem_BBoxOnly,
    renderRLayer_BBoxOnly,
    renderRPage_BBoxOnly,

    -- * render using buf
    renderRBkg_Buf,
    renderRLayer_InBBoxBuf,
-}

{-
import Control.Lens
import Control.Monad.State hiding (mapM, mapM_)
import Data.Foldable
-- from hoodle-platform

import Data.Hoodle.BBox
import Data.Hoodle.Generic
import Data.Hoodle.Predefined
import Data.Hoodle.Simple
import qualified Data.Map as M
-- from this package
import Graphics.Hoodle.Render
import Graphics.Hoodle.Render.Highlight
import Graphics.Hoodle.Render.Type
import qualified Graphics.Rendering.Cairo as Cairo
--
import Prelude hiding (concatMap, curry, mapM, mapM_, uncurry)

-----
-- Dummy (for testing)
-----

renderRBkg_Dummy :: RenderCache -> (RBackground, Dimension, Maybe Xform4Page) -> Cairo.Render ()
renderRBkg_Dummy _ (_, Dim w h, _) = do
  Cairo.setSourceRGBA 1 1 1 1
  Cairo.rectangle 0 0 w h
  Cairo.fill

-----------
-- NoPDF --
-----------

-- | render background without pdf
renderRBkg_NoPDF :: RenderCache -> (RBackground, Dimension, Maybe Xform4Page) -> Cairo.Render ()
renderRBkg_NoPDF cache r@(RBkgSmpl _ _ _, _, _) = renderRBkg cache r >> return ()
renderRBkg_NoPDF _ (RBkgPDF _ _ _ _ _, _, _) = return ()
renderRBkg_NoPDF _ (RBkgEmbedPDF _ _ _, _, _) = return ()

--------------
-- BBoxOnly --
--------------

-- | render only bounding box of a StrokeBBox
renderStrkBBx_BBoxOnly :: BBoxed Stroke -> Cairo.Render ()
renderStrkBBx_BBoxOnly sbbox = do
  let s = bbxed_content sbbox
  case M.lookup (stroke_color s) predefined_pencolor of
    Just (r, g, b, a) -> Cairo.setSourceRGBA r g b a
    Nothing -> Cairo.setSourceRGBA 0 0 0 1
  Cairo.setSourceRGBA 0 0 0 1
  Cairo.setLineWidth (stroke_width s)
  let BBox (x1, y1) (x2, y2) = getBBox sbbox
  Cairo.rectangle x1 y1 (x2 - x1) (y2 - y1)
  Cairo.stroke

-- |
renderImgBBx_BBoxOnly :: BBoxed Image -> Cairo.Render ()
renderImgBBx_BBoxOnly ibbox = do
  Cairo.setSourceRGBA 0 0 0 1
  Cairo.setLineWidth 10
  let BBox (x1, y1) (x2, y2) = getBBox ibbox
  Cairo.rectangle x1 y1 (x2 - x1) (y2 - y1)
  Cairo.stroke

renderSVGBBx_BBoxOnly :: BBoxed SVG -> Cairo.Render ()
renderSVGBBx_BBoxOnly svg = do
  Cairo.setSourceRGBA 0 0 0 1
  Cairo.setLineWidth 10
  let BBox (x1, y1) (x2, y2) = getBBox svg
  Cairo.rectangle x1 y1 (x2 - x1) (y2 - y1)
  Cairo.stroke

renderLnkBBx_BBoxOnly :: BBoxed Link -> Cairo.Render ()
renderLnkBBx_BBoxOnly lnk = do
  Cairo.setSourceRGBA 0 0 0 1
  Cairo.setLineWidth 10
  let BBox (x1, y1) (x2, y2) = getBBox lnk
  Cairo.rectangle x1 y1 (x2 - x1) (y2 - y1)
  Cairo.stroke

-- |
renderRItem_BBoxOnly :: RenderCache -> RItem -> Cairo.Render ()
renderRItem_BBoxOnly _ (RItemStroke sbbox) = renderStrkBBx_BBoxOnly sbbox
renderRItem_BBoxOnly _ (RItemImage ibbox _) = renderImgBBx_BBoxOnly ibbox
renderRItem_BBoxOnly _ (RItemSVG svg _) = renderSVGBBx_BBoxOnly svg
renderRItem_BBoxOnly _ (RItemLink lnk _) = renderLnkBBx_BBoxOnly lnk
renderRItem_BBoxOnly _ (RItemAnchor anc _) = (renderHltBBox . getBBox) anc

-- |
renderRLayer_BBoxOnly :: RenderCache -> RLayer -> Cairo.Render ()
renderRLayer_BBoxOnly cache = mapM_ (renderRItem_BBoxOnly cache) . view gitems

-- | render only bounding box of a StrokeBBox
renderRPage_BBoxOnly :: RenderCache -> RPage -> Cairo.Render ()
renderRPage_BBoxOnly cache page = do
  let dim = view gdimension page
      bkg = view gbackground page
      lyrs = view glayers page
  renderRBkg_NoPDF cache (bkg, dim, Nothing)
  mapM_ (renderRLayer_BBoxOnly cache) lyrs
-}
