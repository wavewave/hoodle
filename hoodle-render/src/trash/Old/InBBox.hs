{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.InBBox 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- render hoodle within a bounding box by hittest
-- 
-----------------------------------------------------------------------------

module Graphics.Hoodle.Render.InBBox where

import           Control.Lens 
import           Data.ByteString hiding (filter,concatMap)
import           Data.Foldable
import qualified Data.Map as M
import           Data.Monoid
import           Graphics.Rendering.Cairo
-- from hoodle-platform 
import Data.Hoodle.Generic
import Data.Hoodle.Simple
import Data.Hoodle.BBox
import Data.Hoodle.Predefined 
-- from this package
import Graphics.Hoodle.Render.Background 
import Graphics.Hoodle.Render.Simple 
import Graphics.Hoodle.Render.Type 
import Graphics.Hoodle.Render.Type.Background
import Graphics.Hoodle.Render.Type.HitTest
-- import Graphics.Hoodle.Render.Type.Item
import Graphics.Hoodle.Render.Util 
import Graphics.Hoodle.Render.Util.HitTest

-- 
import Prelude hiding (fst,snd,curry,uncurry,mapM_,concatMap)





{-
cairoDrawPageBBox :: Maybe BBox -> TPageBBoxMap -> Render ()
cairoDrawPageBBox mbbox page = do 
    cairoDrawBackgroundBBox mbbox (gdimension page) (gbackground page) 
    mapM_ (cairoDrawLayerBBox mbbox) (glayers page)




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
-}