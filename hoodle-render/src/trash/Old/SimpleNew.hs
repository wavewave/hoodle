{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.SimpleNew
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Graphics.Hoodle.Render.SimpleNew where 

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.State 
import           Control.Monad.Trans
import qualified Data.ByteString.Char8 as S
import qualified Data.Map as M
import           Data.Strict.Tuple hiding (fst,snd)
import           Graphics.Rendering.Cairo
-- 
import           Data.Hoodle.Simple
import           Data.Hoodle.Predefined 
-- 
import           Graphics.Hoodle.Render.Type.HoodleRender 

-- | 
drawOneStroke :: Stroke -> HoodleRender ()
drawOneStroke s = do 
    case s of
      Stroke _ _ w d -> do  
        let opacity = if stroke_tool s == "highlighter" 
                      then predefined_highlighter_opacity
                      else 1.0
        case M.lookup (stroke_color s) predefined_pencolor of
          Just (r,g,b,a) -> lift (setSourceRGBA r g b (a*opacity))
          Nothing -> lift (setSourceRGBA 0 0 0 1)
        lift $ do
          setLineWidth w
          setLineCap LineCapRound
          setLineJoin LineJoinRound
        drawOneStrokeCurve d
        lift $ stroke
      VWStroke _ _ d -> do  
        let opacity = if stroke_tool s == "highlighter" 
                      then predefined_highlighter_opacity
                      else 1.0
        case M.lookup (stroke_color s) predefined_pencolor of
          Just (r,g,b,a) -> lift $ setSourceRGBA r g b (a*opacity) 
          Nothing -> lift $ setSourceRGBA 0 0 0 1
        lift $ setFillRule FillRuleWinding
        drawOneVWStrokeCurve d
        lift $ fill 
      Img _ (x,y) (Dim w h) ->
        lift $ do    
          setSourceRGBA 0 0 0 1
          setLineWidth 10
          rectangle x y w h
          stroke
   
-- | 
drawOneStrokeCurve :: [Pair Double Double] -> HoodleRender ()
drawOneStrokeCurve ((x0 :!: y0) : xs) = do 
  x0 `seq` y0 `seq` lift (moveTo x0 y0)
  mapM_ f xs 
    where f (x :!: y) = x `seq` y `seq` (lift (lineTo x y))
drawOneStrokeCurve [] = return ()

-- | 
drawOneVWStrokeCurve :: [(Double,Double,Double)] -> HoodleRender ()
drawOneVWStrokeCurve [] = return ()
drawOneVWStrokeCurve (_:[]) = return ()
drawOneVWStrokeCurve ((xo,yo,_zo) : xs) = 
    lift $ do moveTo xo yo
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


-- | general background drawing (including pdf file)
cairoDrawBackground :: Page -> HoodleRender () 
cairoDrawBackground page = do 
  let Dim w h = page_dim page 
  case page_bkg page of 
    Background typ col sty -> cairoDrawBkg (Dim w h) (Background typ col sty)
    BackgroundPdf _ _mdomain _mfilename _pagenum -> 
      error "in cairoDrawBackground, pdf drawing is not defined yet"

-- | 
cairoDrawBkg :: Dimension -> Background -> HoodleRender () 
cairoDrawBkg (Dim w h) (Background _typ col sty) = do 
    let c = M.lookup col predefined_bkgcolor  
    case c of 
      Just (r,g,b,_a) -> lift $ setSourceRGB r g b 
      Nothing        -> lift $ setSourceRGB 1 1 1 
    lift $ do 
      rectangle 0 0 w h 
      fill
    cairoDrawRuling w h sty
cairoDrawBkg (Dim w h) (BackgroundPdf _typ _mdomain _mfilename _pagenum) = 
    lift $ do 
      setSourceRGBA 1 1 1 1
      rectangle 0 0 w h 
      fill

-- | 
cairoDrawRuling :: Double -> Double -> S.ByteString -> HoodleRender () 
cairoDrawRuling w h style = do
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
      lift $ drawHorizRules
      let (r2,g2,b2,a2) = predefined_RULING_MARGIN_COLOR
      lift $ setSourceRGBA r2 g2 b2 a2 
      lift $ setLineWidth predefined_RULING_THICKNESS
      lift $ moveTo predefined_RULING_LEFTMARGIN 0 
      lift $ lineTo predefined_RULING_LEFTMARGIN h
      lift $ stroke
    "ruled" -> lift $ drawHorizRules 
    "graph" -> do 
      let (r3,g3,b3,a3) = predefined_RULING_COLOR 
      lift $ setSourceRGBA r3 g3 b3 a3 
      lift $ setLineWidth predefined_RULING_THICKNESS
      let drawonegraphvert x = do 
            moveTo x 0 
            lineTo x h
            stroke  
      let drawonegraphhoriz y = do 
            moveTo 0 y
            lineTo w y
            stroke
      lift $ mapM_ drawonegraphvert  [0,predefined_RULING_GRAPHSPACING..w-1] 
      lift $ mapM_ drawonegraphhoriz [0,predefined_RULING_GRAPHSPACING..h-1]
    _ -> return ()     

-- |
cairoDrawPage :: Page -> HoodleRender ()
cairoDrawPage page = do 
  let strokes = (layer_strokes . (!!0) . page_layers) page 
  cairoDrawBackground page
  lift $ setLineCap LineCapRound
  lift $ setLineJoin LineJoinRound
  mapM_ drawOneStroke strokes
  lift $ stroke


