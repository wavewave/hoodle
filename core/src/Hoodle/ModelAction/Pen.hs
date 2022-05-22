{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : Hoodle.ModelAction.Pen
-- Copyright   : (c) 2011-2015 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
module Hoodle.ModelAction.Pen where

import Control.Lens (view)
import Control.Monad (when)
import Data.Foldable
-- from hoodle-platform
import Data.Hoodle.Simple
import Data.Sequence hiding (drop, take)
import Data.Strict.Tuple hiding (uncurry)
import qualified Graphics.Rendering.Cairo as Cairo
-- from this package
import Hoodle.Type.Canvas
import Hoodle.Type.Enum

--

data TempRender a = TempRender
  { tempSurfaceSrc :: Cairo.Surface,
    tempSurfaceTgt :: Cairo.Surface,
    widthHeight :: (Double, Double),
    tempInfo :: a
  }

-- | update the content of temp selection. should not be often updated
updateTempRender :: TempRender a -> Cairo.Render () -> Bool -> IO ()
updateTempRender temprender renderfunc isFullErase =
  Cairo.renderWith (tempSurfaceSrc temprender) $ do
    when isFullErase $ do
      let (cw, ch) = widthHeight temprender
      Cairo.setSourceRGBA 0.5 0.5 0.5 1
      Cairo.rectangle 0 0 cw ch
      Cairo.fill
    renderfunc

-- |
createNewStroke :: PenInfo -> Seq (Double, Double, Double) -> Stroke
createNewStroke pinfo pdraw =
  let ptype = view penType pinfo
      pcolor = view (currentTool . penColor) pinfo
      pcolname = convertPenColorToByteString pcolor
      pwidth = view (currentTool . penWidth) pinfo
      pvwpen = view variableWidthPen pinfo
      ptool = case ptype of
        PenWork -> "pen"
        HighlighterWork -> "highlighter"
        _ -> error "error in createNewStroke"

      newstroke =
        case pvwpen of
          False ->
            Stroke
              { stroke_tool = ptool,
                stroke_color = pcolname,
                stroke_width = pwidth,
                stroke_data = map (\(x, y, _) -> x :!: y) . toList $ pdraw
              }
          True ->
            VWStroke
              { stroke_tool = ptool,
                stroke_color = pcolname,
                stroke_vwdata = map (\(x, y, z) -> (x, y, pwidth * z)) . toList $ pdraw
              }
   in newstroke
