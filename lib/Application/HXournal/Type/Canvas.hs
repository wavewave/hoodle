{-# LANGUAGE TemplateHaskell #-}

module Application.HXournal.Type.Canvas where

import Application.HXournal.Type.Enum 
import Data.Sequence
import qualified Data.IntMap as M

import Control.Category
import Data.Label 
import Prelude hiding ((.), id)

import Graphics.Xournal.Type
import Graphics.Xournal.Type.Map
import Graphics.Xournal.Type.Select

import Graphics.UI.Gtk hiding (get,set)

type CanvasId = Int 

data PenDraw = PenDraw { _points :: Seq (Double,Double) } 
             deriving (Show)
                      
emptyPenDraw :: PenDraw
emptyPenDraw = PenDraw empty

data PageMode = Continous | OnePage
              deriving (Show,Eq) 

data ZoomMode = Original | FitWidth | FitHeight | Zoom Double 
              deriving (Show,Eq)

data ViewInfo = ViewInfo { _pageMode :: PageMode
                         , _zoomMode :: ZoomMode
                         , _viewPortOrigin :: (Double,Double)
                         , _pageDimension :: (Double,Double) 
                         }
                deriving (Show)

data CanvasInfo = CanvasInfo { _canvasId :: CanvasId
                             , _drawArea :: DrawingArea
                             , _scrolledWindow :: ScrolledWindow
                             , _viewInfo :: ViewInfo 
                             , _currentPageNum :: Int
                             , _currentPage :: Either PageBBoxMap TempPageSelect 
                             , _horizAdjustment :: Adjustment
                             , _vertAdjustment :: Adjustment 
                             }


type CanvasInfoMap = M.IntMap CanvasInfo

data PenType = PenWork 
             | HighlighterWork 
             | EraserWork 
             | TextWork 
             deriving (Show,Eq)

data PenInfo = PenInfo { _penType :: PenType
                       , _penWidth :: Double
                       , _penColor :: PenColor } 
             deriving (Show) 


$(mkLabels [''PenDraw, ''ViewInfo, ''PenInfo, ''CanvasInfo])