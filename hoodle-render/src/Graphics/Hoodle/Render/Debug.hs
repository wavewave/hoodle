{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.Debug
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- collection of rendering routine 
--
-----------------------------------------------------------------------------


module Graphics.Hoodle.Render.Debug
(      
-- * dummy rendering 
  renderRBkg_Dummy  
-- * nopdf
, renderRBkg_NoPDF
-- * render only bbox (for debug purpose)
, renderStrkBBx_BBoxOnly
, renderImgBBx_BBoxOnly
, renderRItem_BBoxOnly
, renderRLayer_BBoxOnly
, renderRPage_BBoxOnly
-- * render using buf 
, renderRBkg_Buf
, renderRLayer_InBBoxBuf
) where

import           Control.Lens 
import           Control.Monad.State hiding (mapM,mapM_)
import           Data.Foldable
import qualified Data.Map as M
import qualified Graphics.Rendering.Cairo as Cairo
-- from hoodle-platform 
import           Data.Hoodle.Generic
import           Data.Hoodle.Simple
import           Data.Hoodle.BBox
import           Data.Hoodle.Predefined 
-- from this package
import           Graphics.Hoodle.Render
import           Graphics.Hoodle.Render.Highlight
import           Graphics.Hoodle.Render.Type 
-- 
import Prelude hiding (curry,uncurry,mapM,mapM_,concatMap)

----- 
-- Dummy (for testing) 
-----

renderRBkg_Dummy :: (RBackground,Dimension) -> Cairo.Render ()
renderRBkg_Dummy (_,Dim w h) = do 
    Cairo.setSourceRGBA 1 1 1 1
    Cairo.rectangle 0 0 w h 
    Cairo.fill 

-----------
-- NoPDF -- 
-----------

-- | render background without pdf 
renderRBkg_NoPDF :: (RBackground,Dimension) -> Cairo.Render ()
renderRBkg_NoPDF r@(RBkgSmpl _ _ _,_) = renderRBkg r >> return ()
renderRBkg_NoPDF (RBkgPDF _ _ _ _ _,_) = return ()
renderRBkg_NoPDF (RBkgEmbedPDF _ _ _,_) = return ()

--------------
-- BBoxOnly --
--------------

-- | render only bounding box of a StrokeBBox      
renderStrkBBx_BBoxOnly :: BBoxed Stroke -> Cairo.Render () 
renderStrkBBx_BBoxOnly sbbox = do  
    let s = bbxed_content sbbox
    case M.lookup (stroke_color s) predefined_pencolor of 
      Just (r,g,b,a) -> Cairo.setSourceRGBA r g b a
      Nothing -> Cairo.setSourceRGBA 0 0 0 1 
    Cairo.setSourceRGBA 0 0 0 1
    Cairo.setLineWidth (stroke_width s) 
    let BBox (x1,y1) (x2,y2) = getBBox sbbox
    Cairo.rectangle x1 y1 (x2-x1) (y2-y1)
    Cairo.stroke
  
-- |     
renderImgBBx_BBoxOnly :: BBoxed Image -> Cairo.Render ()
renderImgBBx_BBoxOnly ibbox = do 
    Cairo.setSourceRGBA 0 0 0 1
    Cairo.setLineWidth 10
    let BBox (x1,y1) (x2,y2) = getBBox ibbox
    Cairo.rectangle x1 y1 (x2-x1) (y2-y1)
    Cairo.stroke

renderSVGBBx_BBoxOnly :: BBoxed SVG -> Cairo.Render ()
renderSVGBBx_BBoxOnly svg = do 
    Cairo.setSourceRGBA 0 0 0 1
    Cairo.setLineWidth 10
    let BBox (x1,y1) (x2,y2) = getBBox svg
    Cairo.rectangle x1 y1 (x2-x1) (y2-y1)
    Cairo.stroke

renderLnkBBx_BBoxOnly :: BBoxed Link -> Cairo.Render ()
renderLnkBBx_BBoxOnly lnk = do 
    Cairo.setSourceRGBA 0 0 0 1
    Cairo.setLineWidth 10
    let BBox (x1,y1) (x2,y2) = getBBox lnk
    Cairo.rectangle x1 y1 (x2-x1) (y2-y1)
    Cairo.stroke

-- | 
renderRItem_BBoxOnly :: RItem -> Cairo.Render () 
renderRItem_BBoxOnly (RItemStroke sbbox) = renderStrkBBx_BBoxOnly sbbox
renderRItem_BBoxOnly (RItemImage ibbox _) = renderImgBBx_BBoxOnly ibbox
renderRItem_BBoxOnly (RItemSVG svg _) = renderSVGBBx_BBoxOnly svg
renderRItem_BBoxOnly (RItemLink lnk _) = renderLnkBBx_BBoxOnly lnk
renderRItem_BBoxOnly itm@(RItemAnchor _) = (renderHltBBox . getBBox) itm

-- | 
renderRLayer_BBoxOnly :: RLayer -> Cairo.Render ()
renderRLayer_BBoxOnly = mapM_  renderRItem_BBoxOnly . view gitems


  
-- | render only bounding box of a StrokeBBox      
renderRPage_BBoxOnly :: RPage -> Cairo.Render ()  
renderRPage_BBoxOnly page = do
    let dim = view gdimension page
        bkg = view gbackground page 
        lyrs =  view glayers page
    renderRBkg_NoPDF (bkg,dim)
    mapM_ renderRLayer_BBoxOnly lyrs


