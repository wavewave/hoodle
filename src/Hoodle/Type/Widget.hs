-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Type.Widget
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Types for Widgets 
-- 
-----------------------------------------------------------------------------

module Hoodle.Type.Widget where

import Control.Lens (Simple,Lens,lens) 
-- 
import Hoodle.Type.PageArrangement 

-- | 
data CanvasWidgets = 
  CanvasWidgets { _testWidgetPosition :: CanvasCoordinate
                , _widgetConfig :: WidgetConfig 
                }   

-- | 
testWidgetPosition :: Simple Lens CanvasWidgets CanvasCoordinate
testWidgetPosition = lens _testWidgetPosition (\f a -> f { _testWidgetPosition = a} )

-- | default hoodle widgets
defaultCanvasWidgets :: CanvasWidgets
defaultCanvasWidgets = 
  CanvasWidgets
  { _testWidgetPosition = CvsCoord (100,100)
  , _widgetConfig = defaultWidgetConfig 
  }   


data WidgetConfig = WidgetConfig { _doesUsePanZoom :: Bool } 

-- | flag for pan zoom widget 
doesUsePanZoom :: Simple Lens WidgetConfig Bool 
doesUsePanZoom = lens _doesUsePanZoom (\f a -> f {_doesUsePanZoom = a})

-- | default widget configuration 
defaultWidgetConfig :: WidgetConfig 
defaultWidgetConfig = WidgetConfig { _doesUsePanZoom = True } 


-- | widget config lens 
widgetConfig :: Simple Lens CanvasWidgets WidgetConfig
widgetConfig = lens _widgetConfig (\f a -> f { _widgetConfig =a } )
