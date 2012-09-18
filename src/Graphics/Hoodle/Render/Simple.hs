{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- {-# LANGUAGE ExistentialQuantification, OverloadedStrings, 
--              FlexibleInstances, FlexibleContexts,  
--              TypeFamilies, CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.Simple
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Simplest rendering of hoodle
--
-----------------------------------------------------------------------------

module Graphics.Hoodle.Render.Simple where

import           Control.Applicative 
import           Control.Lens
import           Control.Monad.State hiding (mapM_)
-- import           Data.ByteString hiding (putStrLn)
import qualified Data.ByteString as S
import           Data.Foldable 
import qualified Data.Map as M
import           Data.Strict.Tuple hiding (fst,snd)
import           Graphics.Rendering.Cairo
--
-- from hoodle-platform
import Data.Hoodle.Predefined 
import Data.Hoodle.Simple
-- from this package
--- import Graphics.Hoodle.Render.BBox 
-- import Graphics.Hoodle.Render.Generic
--
import Prelude hiding (mapM_)


-- | 
drawStrokeCurve :: [Pair Double Double] -> Render ()
drawStrokeCurve ((x0 :!: y0) : xs) = do 
  x0 `seq` y0 `seq` moveTo x0 y0
  mapM_ f xs 
    where f (x :!: y) = x `seq` y `seq` lineTo x y 
drawStrokeCurve [] = return ()

-- | 
drawVWStrokeCurve :: [(Double,Double,Double)] -> Render ()
drawVWStrokeCurve [] = return ()
drawVWStrokeCurve (_:[]) = return ()
drawVWStrokeCurve ((xo,yo,_zo) : xs) = do 
    moveTo xo yo
    let ((xlast,ylast,_zlast):rxs) = reverse xs 
    foldM_ forward (xo,yo) xs 
    foldM_ backward (xlast,ylast) rxs 
  where (dx,dy) = (,) <$> fst <*> snd $ predefinedPenShapeAspectXY
        dir (x,y) = x * dy - y * dx
        forward (x0,y0) (x,y,z) = do if (dir (x-x0,y-y0) > 0) 
                                       then lineTo (x+0.5*dx*z) (y+0.5*dy*z)
                                       else lineTo (x-0.5*dx*z) (y-0.5*dy*z) 
                                     return (x,y)       
        backward (x0,y0) (x,y,z) = do if (dir (x-x0,y-y0) < 0) 
                                        then lineTo (x-0.5*dx*z) (y-0.5*dy*z)
                                        else lineTo (x+0.5*dx*z) (y+0.5*dy*z)
                                      return (x,y)


-- | render stroke 
renderStrk :: Stroke -> Render ()
renderStrk s@(Stroke _ _ w d) = do 
    let opacity = if stroke_tool s == "highlighter" 
                  then predefined_highlighter_opacity
                  else 1.0
    case M.lookup (stroke_color s) predefined_pencolor of
      Just (r,g,b,a) -> setSourceRGBA r g b (a*opacity) 
      Nothing -> setSourceRGBA 0 0 0 1
    setLineWidth w
    setLineCap LineCapRound
    setLineJoin LineJoinRound
    drawStrokeCurve d
    stroke
renderStrk s@(VWStroke _ _ d) = do 
    let opacity = if stroke_tool s == "highlighter" 
                  then predefined_highlighter_opacity
                  else 1.0
    case M.lookup (stroke_color s) predefined_pencolor of
      Just (r,g,b,a) -> setSourceRGBA r g b (a*opacity) 
      Nothing -> setSourceRGBA 0 0 0 1
    setFillRule FillRuleWinding
    drawVWStrokeCurve d
    fill 

-- | render image 
renderImg :: Image -> Render () 
renderImg (Image _ (x,y) (Dim w h)) = do  
      setSourceRGBA 0 0 0 1
      setLineWidth 10
      rectangle x y w h
      stroke


-- | render item 
renderItm :: Item -> Render () 
renderItm (ItemStroke strk) = renderStrk strk
renderItm (ItemImage img) = renderImg img



-- | 
renderBkg :: (Background,Dimension) -> Render () 
renderBkg (Background _typ col sty,Dim w h) = do 
    let c = M.lookup col predefined_bkgcolor  
    case c of 
      Just (r,g,b,_a) -> setSourceRGB r g b 
      Nothing        -> setSourceRGB 1 1 1 
    rectangle 0 0 w h 
    fill
    drawRuling w h sty
renderBkg (BackgroundPdf _ _ _ _,Dim w h) = do 
    setSourceRGBA 1 1 1 1
    rectangle 0 0 w h 
    fill


-- | 
drawRuling :: Double -> Double -> S.ByteString -> Render () 
drawRuling w h style = do
  let drawHorizRules = do 
      let (r,g,b,a) = predefined_RULING_COLOR         
      setSourceRGBA r g b a 
      setLineWidth predefined_RULING_THICKNESS
      let drawonerule y = do 
            moveTo 0 y 
            lineTo w y
            stroke  
      mapM_ drawonerule [ predefined_RULING_TOPMARGIN 
                        , predefined_RULING_TOPMARGIN+predefined_RULING_SPACING
                        .. 
                        h-1 ]
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
      let drawonegraphvert x = do 
            moveTo x 0 
            lineTo x h
            stroke  
      let drawonegraphhoriz y = do 
            moveTo 0 y
            lineTo w y
            stroke
      mapM_ drawonegraphvert  [0,predefined_RULING_GRAPHSPACING..w-1] 
      mapM_ drawonegraphhoriz [0,predefined_RULING_GRAPHSPACING..h-1]
    _ -> return ()     


-- |
renderPage :: Page -> Render ()
renderPage page = do 
  let itms = (view items . (!!0) . view layers) page 
  renderBkg (view background page,view dimension page)
  setLineCap LineCapRound
  setLineJoin LineJoinRound
  mapM_ renderItm itms
  stroke

