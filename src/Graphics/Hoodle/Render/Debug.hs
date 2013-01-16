{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.Debug
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
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
-- * render in bbox using non R-structure 
, renderBkg_InBBox
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
import           Graphics.Rendering.Cairo
-- from hoodle-platform 
import Data.Hoodle.Generic
import Data.Hoodle.Simple
import Data.Hoodle.BBox
import Data.Hoodle.Predefined 
-- #ifdef POPPLER
-- import qualified Graphics.UI.Gtk.Poppler.Page as PopplerPage
-- #endif
-- from this package
import Graphics.Hoodle.Render
import Graphics.Hoodle.Render.Type 
-- 
import Prelude hiding (curry,uncurry,mapM,mapM_,concatMap)

----- 
-- Dummy (for testing) 
-----

renderRBkg_Dummy :: (RBackground,Dimension) -> Render () 
renderRBkg_Dummy (_,Dim w h) = do 
    setSourceRGBA 1 1 1 1
    rectangle 0 0 w h 
    fill 

-----------
-- NoPDF -- 
-----------

-- | render background without pdf 
renderRBkg_NoPDF :: (RBackground,Dimension) -> Render ()
renderRBkg_NoPDF r@(RBkgSmpl _ _ _,_) = renderRBkg r >> return ()
renderRBkg_NoPDF (RBkgPDF _ _ _ _ _,_) = return ()

--------------
-- BBoxOnly --
--------------

-- | render only bounding box of a StrokeBBox      
renderStrkBBx_BBoxOnly :: StrokeBBox -> Render () 
renderStrkBBx_BBoxOnly sbbox = do  
    let s = strkbbx_strk sbbox
    case M.lookup (stroke_color s) predefined_pencolor of 
      Just (r,g,b,a) -> setSourceRGBA r g b a
      Nothing -> setSourceRGBA 0 0 0 1 
    setSourceRGBA 0 0 0 1
    setLineWidth (stroke_width s) 
    let BBox (x1,y1) (x2,y2) = strkbbx_bbx sbbox
    rectangle x1 y1 (x2-x1) (y2-y1)
    stroke
  
-- |     
renderImgBBx_BBoxOnly :: ImageBBox -> Render () 
renderImgBBx_BBoxOnly ibbox = do 
    setSourceRGBA 0 0 0 1
    setLineWidth 10
    let BBox (x1,y1) (x2,y2) = imgbbx_bbx ibbox
    rectangle x1 y1 (x2-x1) (y2-y1)
    stroke

renderSVGBBx_BBoxOnly :: SVGBBox -> Render () 
renderSVGBBx_BBoxOnly svg = do 
    setSourceRGBA 0 0 0 1
    setLineWidth 10
    let BBox (x1,y1) (x2,y2) = getBBox svg
    rectangle x1 y1 (x2-x1) (y2-y1)
    stroke

    

-- | 
renderRItem_BBoxOnly :: RItem -> Render () 
renderRItem_BBoxOnly (RItemStroke sbbox) = renderStrkBBx_BBoxOnly sbbox
renderRItem_BBoxOnly (RItemImage ibbox _) = renderImgBBx_BBoxOnly ibbox
renderRItem_BBoxOnly (RItemSVG svg _) = renderSVGBBx_BBoxOnly svg


-- | 
renderRLayer_BBoxOnly :: RLayer -> Render ()
renderRLayer_BBoxOnly = mapM_  renderRItem_BBoxOnly . view gitems


  
-- | render only bounding box of a StrokeBBox      
renderRPage_BBoxOnly :: RPage -> Render ()  
renderRPage_BBoxOnly page = do
    let dim = view gdimension page
        bkg = view gbackground page 
        lyrs =  view glayers page
    renderRBkg_NoPDF (bkg,dim)
    mapM_ renderRLayer_BBoxOnly lyrs


