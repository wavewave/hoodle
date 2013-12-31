{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.BBoxOnly 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- collection of rendering routine that draws bounding box only
--
-----------------------------------------------------------------------------


module Graphics.Hoodle.Render.BBoxOnly where

import           Control.Lens 
import           Data.Foldable
import qualified Data.Map as M
import           Graphics.Rendering.Cairo
-- from hoodle-platform 
import Data.Hoodle.Generic
import Data.Hoodle.Simple
import Data.Hoodle.BBox
import Data.Hoodle.Predefined 
-- from this package
import Graphics.Hoodle.Render.Background 
import Graphics.Hoodle.Render.Type 
import Graphics.Hoodle.Render.Type.Background
-- import Graphics.Hoodle.Render.Type.Item
-- 
import Prelude hiding (fst,snd,curry,uncurry,mapM_,concatMap)


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
    
{-
-- | 
renderRItem_BBoxOnly :: RItem -> Render () 
renderRItem_BBoxOnly (RItemStroke sbbox) = renderStrkBBx_BBoxOnly sbbox
renderRItem_BBoxOnly (RItemImage ibbox _) = renderImgBBx_BBoxOnly ibbox
-}

-- | 
renderRLayer_BBoxOnly :: RLayer -> Render ()
renderRLayer_BBoxOnly = mapM_ renderStrkBBx_BBoxOnly . view gstrokes 
                        -- mapM_  renderRItem_BBoxOnly . view gitems

-- |
renderRBkg_BBoxOnly :: (RBackground,Dimension) -> Render ()
renderRBkg_BBoxOnly r@(RBkgSmpl _ _ _,_) = renderRBkg r
renderRBkg_BBoxOnly (RBkgPDF _ _ _ _ _,_) = return ()

  
-- | render only bounding box of a StrokeBBox      
renderPage_BBoxOnly :: RPage -> Render ()  
renderPage_BBoxOnly page = do
    let dim = view gdimension page
        bkg = view gbackground page 
        lyrs =  view glayers page
    -- cairoDrawBackground (toPage id page)
    renderRBkg_BBoxOnly (bkg,dim)
    mapM_ renderRLayer_BBoxOnly lyrs



{-
cairoOneStrokeSelected :: StrokeBBox -> Render ()
cairoOneStrokeSelected sbbox = do 
  let s = gToStroke sbbox 
  case s of     
    Img _ _ _ -> cairoOneStrokeBBoxOnly sbbox 
    _ -> do     
      case M.lookup (stroke_color s) predefined_pencolor of 
        Just (r,g,b,a) -> setSourceRGBA r g b a
        Nothing -> setSourceRGBA 0 0 0 1 
      case s of
        Stroke _ _ w d -> do  
          setLineWidth (w * 4.0) 
          setLineCap LineCapRound
          setLineJoin LineJoinRound
          drawOneStrokeCurve d
          stroke
          setSourceRGBA 1 1 1 1
          setLineWidth w
          drawOneStrokeCurve . stroke_data $ s 
          stroke
        VWStroke _ _ d -> do  
          setFillRule FillRuleWinding
          drawOneVWStrokeCurve $ map (\(x,y,z)->(x,y,4*z)) d
          fill  
          setSourceRGBA 1 1 1 1
          drawOneVWStrokeCurve d     
          fill
        _ -> error "in cairoOneStrokeSelected"
-}    
    
{-
cairoDrawLayerBBoxOnly :: TLayerBBox -> Render () 
cairoDrawLayerBBoxOnly  = mapM_ cairoOneStrokeBBoxOnly . gstrokes 

----

cairoDrawPageBBox :: Maybe BBox -> TPageBBoxMap -> Render ()
cairoDrawPageBBox mbbox page = do 
    cairoDrawBackgroundBBox mbbox (gdimension page) (gbackground page) 
    mapM_ (cairoDrawLayerBBox mbbox) (glayers page)


cairoDrawLayerBBox :: Maybe BBox -> TLayerBBox -> Render () 
cairoDrawLayerBBox mbbox layer = do  
  clipBBox mbbox 
  let hittestbbox = case mbbox of 
                       Nothing -> NotHitted [] 
                                  :- Hitted (gstrokes layer) 
                                  :- Empty 
                       Just bbox -> mkHitTestBBoxBBox bbox (gstrokes layer)
  mapM_ drawOneStroke . map gToStroke . concatMap unHitted  . getB $ hittestbbox
  resetClip

cairoDrawBackgroundBBox :: Maybe BBox -> Dimension -> Background -> Render ()
cairoDrawBackgroundBBox mbbox dim@(Dim w h) (Background typ col sty) = do 
    let mbbox2 = toMaybe $ fromMaybe mbbox `mappend` (Intersect (Middle (dimToBBox dim)))
    case mbbox2 of 
      Nothing -> cairoDrawBkg (Dim w h) (Background typ col sty)
      Just bbox@(BBox (x1,y1) (x2,y2)) -> do 
        let c = M.lookup col predefined_bkgcolor  
        case c of 
          Just (r,g,b,_a) -> setSourceRGB r g b 
          Nothing        -> setSourceRGB 1 1 1 
        rectangle x1 y1 (x2-x1) (y2-y1)
        fill
        cairoDrawRulingBBox bbox w h sty
cairoDrawBackgroundBBox _ _  (BackgroundPdf _ _ _ _) = 
    error "BackgroundPdf in cairoDrawBackgroundBBox"

-}