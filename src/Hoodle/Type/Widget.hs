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

module Hoodle.Type.Widget 
( 
-- * types
  CanvasWidgets      
, WidgetConfig
-- * lenses 
, panZoomWidgetPosition
, layerWidgetPosition
, widgetConfig 
, doesUsePanZoomWidget
, doesUseLayerWidget
-- * defaults 
, defaultCanvasWidgets
, defaultWidgetConfig
) where

import Control.Lens (Simple,Lens,lens) 
-- 
import Hoodle.Type.PageArrangement 

-- | 
data CanvasWidgets = 
  CanvasWidgets { _panZoomWidgetPosition :: CanvasCoordinate
                , _layerWidgetPosition :: CanvasCoordinate
                , _widgetConfig :: WidgetConfig 
                }   

-- | 
panZoomWidgetPosition :: Simple Lens CanvasWidgets CanvasCoordinate
panZoomWidgetPosition = lens _panZoomWidgetPosition (\f a -> f { _panZoomWidgetPosition = a })

-- | 
layerWidgetPosition :: Simple Lens CanvasWidgets CanvasCoordinate 
layerWidgetPosition = lens _layerWidgetPosition (\f a -> f { _layerWidgetPosition = a })

-- | default hoodle widgets
defaultCanvasWidgets :: CanvasWidgets
defaultCanvasWidgets = 
  CanvasWidgets
  { _panZoomWidgetPosition = CvsCoord (100,100)
  , _layerWidgetPosition = CvsCoord (100,300)
  , _widgetConfig = defaultWidgetConfig 
  }   


data WidgetConfig = WidgetConfig { _doesUsePanZoomWidget :: Bool 
                                 , _doesUseLayerWidget :: Bool 
                                 } 

-- | flag for pan zoom widget 
doesUsePanZoomWidget :: Simple Lens WidgetConfig Bool 
doesUsePanZoomWidget = lens _doesUsePanZoomWidget (\f a -> f {_doesUsePanZoomWidget = a})

-- | flag for layer widget 
doesUseLayerWidget :: Simple Lens WidgetConfig Bool 
doesUseLayerWidget = lens _doesUseLayerWidget (\f a -> f {_doesUseLayerWidget = a}) 


-- | default widget configuration 
defaultWidgetConfig :: WidgetConfig 
defaultWidgetConfig = WidgetConfig { _doesUsePanZoomWidget = True 
                                   , _doesUseLayerWidget = True
                                   } 


-- | widget config lens 
widgetConfig :: Simple Lens CanvasWidgets WidgetConfig
widgetConfig = lens _widgetConfig (\f a -> f { _widgetConfig =a } )
