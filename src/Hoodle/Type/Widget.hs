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
  WidgetItem(..)
, CanvasWidgets      
, WidgetConfig
, LayerWidgetConfig 
-- * lenses 
, panZoomWidgetPosition
, panZoomWidgetTouchIsZoom
, panZoomWidgetConfig 
, layerWidgetConfig
, layerWidgetPosition
, layerWidgetShowContent
, widgetConfig 
, doesUsePanZoomWidget
, doesUseLayerWidget
-- * defaults 
, defaultCanvasWidgets
, defaultLWConfig 
, defaultWidgetConfig
-- * utility
, allWidgets
) where

import Control.Lens (Simple,Lens,lens) 
-- 
import Hoodle.Type.PageArrangement 


-- | 
data WidgetItem = PanZoomWidget | LayerWidget
                deriving (Show,Eq,Ord)

-- | 
allWidgets = [PanZoomWidget, LayerWidget] 


-- | 
data PanZoomWidgetConfig = PZWConfig { _panZoomWidgetPosition :: CanvasCoordinate
                                     , _panZoomWidgetTouchIsZoom :: Bool  
                                     } 
                           
-- | 
panZoomWidgetPosition :: Simple Lens PanZoomWidgetConfig CanvasCoordinate
panZoomWidgetPosition = lens _panZoomWidgetPosition (\f a -> f { _panZoomWidgetPosition = a })

-- | 
panZoomWidgetTouchIsZoom :: Simple Lens PanZoomWidgetConfig Bool 
panZoomWidgetTouchIsZoom = lens _panZoomWidgetTouchIsZoom (\f a -> f { _panZoomWidgetTouchIsZoom = a})

-- |
data LayerWidgetConfig = LWConfig { _layerWidgetPosition :: CanvasCoordinate
                                  , _layerWidgetShowContent :: Bool
                                  }
-- | 
layerWidgetPosition :: Simple Lens LayerWidgetConfig CanvasCoordinate 
layerWidgetPosition = lens _layerWidgetPosition (\f a -> f { _layerWidgetPosition = a })
                         
-- | 
layerWidgetShowContent :: Simple Lens LayerWidgetConfig Bool
layerWidgetShowContent = lens _layerWidgetShowContent (\f a -> f { _layerWidgetShowContent = a})


-- | 
data CanvasWidgets = 
  CanvasWidgets { _panZoomWidgetConfig :: PanZoomWidgetConfig
                , _layerWidgetConfig :: LayerWidgetConfig 
                , _widgetConfig :: WidgetConfig 
                }   

-- | 
panZoomWidgetConfig :: Simple Lens CanvasWidgets PanZoomWidgetConfig 
panZoomWidgetConfig = lens _panZoomWidgetConfig (\f a -> f { _panZoomWidgetConfig = a })

-- | 
layerWidgetConfig :: Simple Lens CanvasWidgets LayerWidgetConfig 
layerWidgetConfig = lens _layerWidgetConfig (\f a -> f { _layerWidgetConfig = a })


-- | default hoodle widgets
defaultCanvasWidgets :: CanvasWidgets
defaultCanvasWidgets = 
  CanvasWidgets
  { _panZoomWidgetConfig = defaultPZWConfig 
  , _layerWidgetConfig = defaultLWConfig 
  , _widgetConfig = defaultWidgetConfig 
  }   

-- | 
defaultPZWConfig :: PanZoomWidgetConfig 
defaultPZWConfig = 
  PZWConfig { _panZoomWidgetPosition = CvsCoord (100,100) 
            , _panZoomWidgetTouchIsZoom = False 
            } 
-- | 
defaultLWConfig :: LayerWidgetConfig 
defaultLWConfig = 
  LWConfig { _layerWidgetPosition = CvsCoord (100,300) 
           , _layerWidgetShowContent = False 
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
defaultWidgetConfig = WidgetConfig { _doesUsePanZoomWidget = False
                                   , _doesUseLayerWidget = False
                                   } 


-- | widget config lens 
widgetConfig :: Simple Lens CanvasWidgets WidgetConfig
widgetConfig = lens _widgetConfig (\f a -> f { _widgetConfig =a } )
