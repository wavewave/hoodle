{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Xournal.Render.BBox 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--

module Graphics.Xournal.Render.BBox where

import Graphics.Rendering.Cairo
import Graphics.Xournal.Render.Simple
import Graphics.Xournal.Render.HitTest
import Graphics.Xournal.Render.Type 

import Data.Xournal.Generic
import Data.Xournal.Map
import Data.Xournal.Simple
import Data.Xournal.BBox
import Data.Xournal.Predefined 

import Data.Foldable
import Data.Monoid 
import qualified Data.Map as M

import Data.ByteString hiding (map, minimum, maximum, concat, concatMap, filter )

import Prelude hiding (fst,snd,curry,uncurry,mapM_,concatMap)

clipBBox :: Maybe BBox -> Render ()
clipBBox (Just (BBox (x1,y1) (x2,y2))) = do {resetClip; rectangle x1 y1 (x2-x1) (y2-y1); clip}
clipBBox Nothing = resetClip 

clearBBox :: Maybe BBox -> Render ()        
clearBBox Nothing = return ()
clearBBox (Just (BBox (x1,y1) (x2,y2))) = do 
    save
    setSourceRGBA 0 0 0 0
    setOperator OperatorSource
    rectangle x1 y1 (x2-x1) (y2-y1) 
    fill
    restore


cairoOneStrokeBBoxOnly :: StrokeBBox -> Render () 
cairoOneStrokeBBoxOnly sbbox = do  
  let s = gToStroke sbbox
  case M.lookup (stroke_color s) predefined_pencolor of 
    Just (r,g,b,a) -> setSourceRGBA r g b a
    Nothing -> setSourceRGBA 0 0 0 1 
  setLineWidth (stroke_width s) 
  let BBox (x1,y1) (x2,y2) = strokebbox_bbox sbbox
  rectangle x1 y1 (x2-x1) (y2-y1)
  stroke
  
cairoDrawPageBBoxOnly :: TPageBBoxMap -> Render ()  
cairoDrawPageBBoxOnly page = do
    let layers =  glayers page
    cairoDrawBackground (toPage id page)
    mapM_ cairoDrawLayerBBoxOnly layers

cairoDrawLayerBBoxOnly :: TLayerBBox -> Render () 
cairoDrawLayerBBoxOnly  = mapM_ cairoOneStrokeBBoxOnly . gstrokes 

----

inflate :: BBox -> Double -> BBox 
inflate (BBox (x1,y1) (x2,y2)) r = BBox (x1-r,y1-r) (x2+r,y2+r)

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

cairoDrawRulingBBox :: BBox -> Double -> Double -> ByteString -> Render () 
cairoDrawRulingBBox (BBox (x1,y1) (x2,y2)) w h style = do
  let drawonerule y = do 
        moveTo x1 y 
        lineTo x2 y
        stroke  
  let drawonegraphvert x = do 
        moveTo x y1 
        lineTo x y2
        stroke  
  let drawonegraphhoriz y = do 
        moveTo x1 y
        lineTo x2 y
        stroke
      fullRuleYs = [ predefined_RULING_TOPMARGIN 
                   , predefined_RULING_TOPMARGIN+predefined_RULING_SPACING
                   .. 
                   h-1 ]
      ruleYs = filter (\y-> (y <= y2) && (y >= y1)) fullRuleYs
      fullGraphXs = [0,predefined_RULING_GRAPHSPACING..w-1]          
      fullGraphYs = [0,predefined_RULING_GRAPHSPACING..h-1]
      graphXs = filter (\x->(x<=x2)&&(x>=x1)) fullGraphXs
      graphYs = filter (\y->(y<=y2)&&(y>=y1)) fullGraphYs 
  
  let drawHorizRules = do 
      let (r,g,b,a) = predefined_RULING_COLOR         
      setSourceRGBA r g b a 
      setLineWidth predefined_RULING_THICKNESS
      mapM_ drawonerule ruleYs
  
  
  case style of 
    "plain" -> return () 
    "lined" -> do 
      drawHorizRules
      let (r2,g2,b2,a2) = predefined_RULING_MARGIN_COLOR
      setSourceRGBA r2 g2 b2 a2 
      setLineWidth predefined_RULING_THICKNESS
      moveTo predefined_RULING_LEFTMARGIN 0 
      lineTo predefined_RULING_LEFTMARGIN h
      stroke
    "ruled" -> drawHorizRules 
    "graph" -> do 
      let (r3,g3,b3,a3) = predefined_RULING_COLOR 
      setSourceRGBA r3 g3 b3 a3 
      setLineWidth predefined_RULING_THICKNESS
      mapM_ drawonegraphvert  graphXs 
      mapM_ drawonegraphhoriz graphYs
    _ -> return ()     
