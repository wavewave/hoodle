-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.ModelAction.Adjustment 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.ModelAction.Adjustment where

-- from other package
import Graphics.UI.Gtk 
-- from hoodle-platform 
import Data.Hoodle.BBox (BBox(..))
import Data.Hoodle.Simple (Dimension(..))
-- from this package
import Hoodle.Type.PageArrangement
import Hoodle.View.Coordinate

-- | adjust values, upper limit and page size according to canvas geometry 

adjustScrollbarWithGeometry :: CanvasGeometry 
                               -> ((Adjustment,Maybe (ConnectId Adjustment))
                                  ,(Adjustment,Maybe (ConnectId Adjustment))) 
                               -> IO ()
adjustScrollbarWithGeometry geometry ((hadj,mconnidh),(vadj,mconnidv)) = do 
  let DesktopDimension (Dim w h) = desktopDim geometry 
      ViewPortBBox (BBox (x0,y0) (x1,y1)) = canvasViewPort geometry 
      xsize = x1-x0
      ysize = y1-y0 
  maybe (return ()) signalBlock mconnidh
  maybe (return ()) signalBlock mconnidv
  adjustmentSetUpper hadj w 
  adjustmentSetUpper vadj h 
  adjustmentSetValue hadj x0 
  adjustmentSetValue vadj y0
  print (xsize,ysize) 
  adjustmentSetPageSize hadj xsize -- (min xsize w)
  adjustmentSetPageSize vadj ysize --  (min ysize h)
  adjustmentSetPageIncrement hadj (xsize*0.9)
  adjustmentSetPageIncrement vadj (ysize*0.9)
  maybe (return ()) signalUnblock mconnidh
  maybe (return ()) signalUnblock mconnidv

-- | 

setAdjustments :: ((Adjustment,Maybe (ConnectId Adjustment))
                  ,(Adjustment,Maybe (ConnectId Adjustment))) 
                  -> (Double,Double) 
                  -> (Double,Double)
                  -> (Double,Double) 
                  -> (Double,Double)
                  -> IO ()
setAdjustments ((hadj,mconnidh),(vadj,mconnidv)) 
               (upperx,uppery) (lowerx,lowery) 
               (valuex,valuey) (pagex,pagey) = do 
    maybe (return ()) signalBlock mconnidh
    maybe (return ()) signalBlock mconnidv
    adjustmentSetUpper hadj upperx 
    adjustmentSetUpper vadj uppery 
    adjustmentSetLower hadj lowerx
    adjustmentSetLower vadj lowery
    adjustmentSetValue hadj valuex
    adjustmentSetValue vadj valuey 
    adjustmentSetPageSize hadj pagex
    adjustmentSetPageSize vadj pagey
    maybe (return ()) signalUnblock mconnidh
    maybe (return ()) signalUnblock mconnidv
    

     
