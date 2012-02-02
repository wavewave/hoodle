-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.ModelAction.Adjustment 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Application.HXournal.ModelAction.Adjustment where

import Application.HXournal.Type.PageArrangement
import Application.HXournal.View.Coordinate
import Data.Xournal.Simple (Dimension(..))
import Data.Xournal.BBox (BBox(..))
import Graphics.UI.Gtk 

-- | adjust values, upper limit and page size according to canvas geometry 

adjustScrollbarWithGeometry :: CanvasGeometry 
                               -> (Adjustment,Adjustment) -> IO ()
adjustScrollbarWithGeometry geometry (hadj,vadj) = do 
  let DesktopDimension (Dim w h) = desktopDim geometry 
      ViewPortBBox (BBox (x0,y0) (x1,y1)) = canvasViewPort geometry 
      xsize = x1-x0
      ysize = y1-y0 
  adjustmentSetUpper hadj w -- (w-xsize)
  adjustmentSetUpper vadj h -- (h-ysize)
  adjustmentSetValue hadj x0 
  adjustmentSetValue vadj y0
  adjustmentSetPageSize hadj (min xsize w)
  adjustmentSetPageSize vadj (min ysize h)
  
  print (w,h,x0,y0,(min xsize w),(min ysize h))
  
-- | 

setAdjustments :: (Adjustment,Adjustment) 
                  -> (Double,Double) 
                  -> (Double,Double)
                  -> (Double,Double) 
                  -> (Double,Double)
                  -> IO ()
setAdjustments (hadj,vadj) (upperx,uppery) (lowerx,lowery) (valuex,valuey) (pagex,pagey) = do 
    adjustmentSetUpper hadj upperx 
    adjustmentSetUpper vadj uppery 
    adjustmentSetLower hadj lowerx
    adjustmentSetLower vadj lowery
    adjustmentSetValue hadj valuex
    adjustmentSetValue vadj valuey 
    adjustmentSetPageSize hadj pagex
    adjustmentSetPageSize vadj pagey
    

     
