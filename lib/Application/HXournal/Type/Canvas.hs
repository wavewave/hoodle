{-# LANGUAGE TemplateHaskell, TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.Type.Canvas 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--

module Application.HXournal.Type.Canvas where

import Application.HXournal.Type.Enum 
import Data.Sequence
import qualified Data.IntMap as M
import Control.Category
import Data.Label 
import Prelude hiding ((.), id)
import Graphics.Xournal.Render.BBoxMapPDF

import Graphics.UI.Gtk hiding (get,set)

import Data.Xournal.Predefined 

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
                             , _currentPage :: Either TPageBBoxMapPDFBuf TTempPageSelectPDFBuf 
                             , _horizAdjustment :: Adjustment
                             , _vertAdjustment :: Adjustment 
                             }


type CanvasInfoMap = M.IntMap CanvasInfo

data PenType = PenWork 
             | HighlighterWork 
             | EraserWork 
             | TextWork 
             deriving (Show,Eq)

data WidthColorStyle = WidthColorStyle { _penWidth :: Double
                                       , _penColor :: PenColor } 
                     deriving (Show)
                       
data PenHighlighterEraserSet = PenHighlighterEraserSet 
                               { _currPen :: WidthColorStyle 
                               , _currHighlighter :: WidthColorStyle 
                               , _currEraser :: WidthColorStyle 
                               , _currText :: WidthColorStyle}
                             deriving (Show) 
                     
data PenInfo = PenInfo { _penType :: PenType
                       -- , _penWidth :: Double
                       -- , _penColor :: PenColor 
                       , _penSet :: PenHighlighterEraserSet } 
             deriving (Show) 

currentTool :: PenInfo :-> WidthColorStyle 
currentTool = lens chooser setter
  where chooser pinfo = case _penType pinfo of
                          PenWork -> _currPen . _penSet $ pinfo
                          HighlighterWork -> _currHighlighter . _penSet $ pinfo
                          EraserWork -> _currEraser . _penSet $ pinfo
                          TextWork -> _currText . _penSet $ pinfo 
        setter wcs pinfo = 
          let pset = _penSet pinfo
              psetnew = case _penType pinfo of 
                          PenWork -> pset { _currPen = wcs }
                          HighlighterWork -> pset { _currHighlighter = wcs }
                          EraserWork -> pset { _currEraser = wcs }
                          TextWork -> pset { _currText = wcs }
          in  pinfo { _penSet = psetnew } 
                                           
defaultPenWCS = WidthColorStyle predefined_medium ColorBlack               
defaultEraserWCS = WidthColorStyle predefined_eraser_medium ColorWhite
defaultTextWCS = defaultPenWCS
defaultHighligherWCS = WidthColorStyle predefined_highlighter_medium ColorYellow


defaultPenInfo :: PenInfo 
defaultPenInfo = 
  PenInfo { _penType = PenWork 
          , _penSet = PenHighlighterEraserSet { _currPen = defaultPenWCS
                                              , _currHighlighter = defaultHighligherWCS
                                              , _currEraser = defaultEraserWCS
                                              , _currText = defaultTextWCS }
          } 
                                           
$(mkLabels [''PenDraw, ''ViewInfo, ''PenInfo, ''PenHighlighterEraserSet, ''WidthColorStyle, ''CanvasInfo])

