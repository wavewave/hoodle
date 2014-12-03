-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.ModelAction.Adjustment 
-- Copyright   : (c) 2011,2012,2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.ModelAction.Adjustment where

-- from other package
import qualified Data.Foldable as F (mapM_)
import qualified Graphics.UI.Gtk as Gtk
-- from hoodle-platform 
import Data.Hoodle.BBox (BBox(..))
import Data.Hoodle.Simple (Dimension(..))
-- from this package
import Hoodle.Type.PageArrangement
import Hoodle.View.Coordinate

-- | adjust values, upper limit and page size according to canvas geometry 

adjustScrollbarWithGeometry :: CanvasGeometry 
                               -> ((Gtk.Adjustment,Maybe (Gtk.ConnectId Gtk.Adjustment))
                                  ,(Gtk.Adjustment,Maybe (Gtk.ConnectId Gtk.Adjustment))) 
                               -> IO ()
adjustScrollbarWithGeometry geometry ((hadj,mconnidh),(vadj,mconnidv)) = do 
  let DesktopDimension (Dim w h) = desktopDim geometry 
      ViewPortBBox (BBox (x0,y0) (x1,y1)) = canvasViewPort geometry 
      xsize = x1-x0
      ysize = y1-y0 
  F.mapM_ Gtk.signalBlock mconnidh
  F.mapM_ Gtk.signalBlock mconnidv
  Gtk.adjustmentSetUpper hadj w 
  Gtk.adjustmentSetUpper vadj h 
  Gtk.adjustmentSetValue hadj x0 
  Gtk.adjustmentSetValue vadj y0
  Gtk.adjustmentSetPageSize hadj xsize -- (min xsize w)
  Gtk.adjustmentSetPageSize vadj ysize --  (min ysize h)
  Gtk.adjustmentSetPageIncrement hadj (xsize*0.9)
  Gtk.adjustmentSetPageIncrement vadj (ysize*0.9)
  F.mapM_ Gtk.signalUnblock mconnidh
  F.mapM_ Gtk.signalUnblock mconnidv

-- | 

setAdjustments :: ((Gtk.Adjustment,Maybe (Gtk.ConnectId Gtk.Adjustment))
                  ,(Gtk.Adjustment,Maybe (Gtk.ConnectId Gtk.Adjustment))) 
                  -> (Double,Double) 
                  -> (Double,Double)
                  -> (Double,Double) 
                  -> (Double,Double)
                  -> IO ()
setAdjustments ((hadj,mconnidh),(vadj,mconnidv)) 
               (upperx,uppery) (lowerx,lowery) 
               (valuex,valuey) (pagex,pagey) = do 
    F.mapM_ Gtk.signalBlock mconnidh
    F.mapM_ Gtk.signalBlock mconnidv
    Gtk.adjustmentSetUpper hadj upperx 
    Gtk.adjustmentSetUpper vadj uppery 
    Gtk.adjustmentSetLower hadj lowerx
    Gtk.adjustmentSetLower vadj lowery
    Gtk.adjustmentSetValue hadj valuex
    Gtk.adjustmentSetValue vadj valuey 
    Gtk.adjustmentSetPageSize hadj pagex
    Gtk.adjustmentSetPageSize vadj pagey
    F.mapM_ Gtk.signalUnblock mconnidh
    F.mapM_ Gtk.signalUnblock mconnidv
    

     
