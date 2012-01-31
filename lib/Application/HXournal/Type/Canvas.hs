{-# LANGUAGE TemplateHaskell, TypeOperators, ExistentialQuantification,
             Rank2Types, GADTs #-}

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
-----------------------------------------------------------------------------

module Application.HXournal.Type.Canvas where

import Application.HXournal.Type.Enum 
import Application.HXournal.Type.Alias 
import Data.Sequence
import qualified Data.IntMap as M
import Control.Applicative ((<*>),(<$>))
import Control.Category
import Data.Label 
import Prelude hiding ((.), id)

import Graphics.UI.Gtk hiding (get,set)
import Data.Xournal.Simple (Dimension(..))
import Data.Xournal.BBox
import Data.Xournal.Predefined 
import Application.HXournal.Type.PageArrangement

import Control.Monad.Identity (Identity(..))

type CanvasId = Int 

data PenDraw = PenDraw { _points :: Seq (Double,Double) } 
             deriving (Show)
                      
emptyPenDraw :: PenDraw
emptyPenDraw = PenDraw empty


data ViewInfo a  = (ViewMode a) => ViewInfo { _zoomMode :: ZoomMode 
                                            , _pageArrangement :: PageArrangement a } 


defaultViewInfoSinglePage :: ViewInfo SinglePage
defaultViewInfoSinglePage = ViewInfo { _zoomMode = Original 
                                     , _pageArrangement = SingleArrangement (PageDimension (Dim 100 100)) (ViewPortBBox (BBox (0,0) (100,100))) } 


zoomMode :: ViewInfo a :-> ZoomMode 
zoomMode = lens _zoomMode (\a f -> f { _zoomMode = a } )


pageArrangement :: ViewInfo a :-> PageArrangement a  
pageArrangement = lens _pageArrangement (\a f -> f { _pageArrangement = a })

data CanvasInfo a = 
    (ViewMode a) => CanvasInfo { _canvasId :: CanvasId
                               , _drawArea :: DrawingArea
                               , _scrolledWindow :: ScrolledWindow
                               , _viewInfo :: ViewInfo a
                               , _currentPageNum :: Int
                               , _currentPage :: Either (Page EditMode) (Page SelectMode)
                               , _horizAdjustment :: Adjustment
                               , _vertAdjustment :: Adjustment 
                               }


canvasId :: CanvasInfo a :-> CanvasId 
canvasId = lens _canvasId (\a f -> f { _canvasId = a })

drawArea :: CanvasInfo a :-> DrawingArea
drawArea = lens _drawArea (\a f -> f { _drawArea = a })

scrolledWindow :: CanvasInfo a :-> ScrolledWindow
scrolledWindow = lens _scrolledWindow (\a f -> f { _scrolledWindow = a })

viewInfo :: CanvasInfo a :-> ViewInfo a 
viewInfo = lens _viewInfo (\a f -> f { _viewInfo = a }) 

currentPageNum :: CanvasInfo a :-> Int 
currentPageNum = lens _currentPageNum (\a f -> f { _currentPageNum = a })

currentPage :: CanvasInfo a :-> Either (Page EditMode) (Page SelectMode)
currentPage = lens _currentPage (\a f -> f { _currentPage = a })

horizAdjustment :: CanvasInfo a :-> Adjustment 
horizAdjustment = lens _horizAdjustment (\a f -> f { _horizAdjustment = a })

vertAdjustment :: CanvasInfo a :-> Adjustment 
vertAdjustment = lens _vertAdjustment (\a f -> f { _vertAdjustment = a })


-- | 

adjustments :: CanvasInfo a :-> (Adjustment,Adjustment) 
adjustments = Lens $ (,) <$> (fst `for` horizAdjustment)
                         <*> (snd `for` vertAdjustment)


data CanvasInfoBox = forall a. (ViewMode a) => CanvasInfoBox (CanvasInfo a) 

getDrawAreaFromBox :: CanvasInfoBox -> DrawingArea 
getDrawAreaFromBox = unboxGet drawArea --  (CanvasInfoBox x) = get drawArea x 

unboxGet :: (forall a. (ViewMode a) => CanvasInfo a :-> b) -> CanvasInfoBox -> b 
unboxGet f (CanvasInfoBox x) = get f x

fmapBox :: (forall a. (ViewMode a) => CanvasInfo a -> CanvasInfo a)
        -> CanvasInfoBox -> CanvasInfoBox
fmapBox f (CanvasInfoBox cinfo) = CanvasInfoBox (f cinfo)


selectBoxAction :: (Monad m) => 
                   (CanvasInfo SinglePage -> m a) 
                -> (CanvasInfo ContinuousSinglePage -> m a) -> CanvasInfoBox -> m a 
selectBoxAction fsingle fcont (CanvasInfoBox cinfo) = 
  case get (pageArrangement.viewInfo) cinfo of 
    SingleArrangement _ _ ->  fsingle cinfo 
    ContinuousSingleArrangement _ _ _ -> fcont cinfo 

selectBox :: (CanvasInfo SinglePage -> CanvasInfo SinglePage)
          -> (CanvasInfo ContinuousSinglePage -> CanvasInfo ContinuousSinglePage)
          -> CanvasInfoBox -> CanvasInfoBox 
selectBox fsingle fcont = 
  let idaction :: CanvasInfoBox -> Identity CanvasInfoBox
      idaction = selectBoxAction (return . CanvasInfoBox . fsingle) (return . CanvasInfoBox . fcont)
  in runIdentity . idaction   

pageArrEitherFromCanvasInfoBox :: CanvasInfoBox 
                 -> Either (PageArrangement SinglePage) (PageArrangement ContinuousSinglePage)
pageArrEitherFromCanvasInfoBox (CanvasInfoBox cinfo) = 
  pageArrEither . get (pageArrangement.viewInfo) $ cinfo 


viewModeBranch :: (CanvasInfo SinglePage -> CanvasInfo SinglePage) 
               -> (CanvasInfo ContinuousSinglePage -> CanvasInfo ContinuousSinglePage) 
               -> CanvasInfo v -> CanvasInfo v 
viewModeBranch fsingle fcont cinfo = 
  case get (pageArrangement.viewInfo) cinfo of 
    SingleArrangement _ _ ->  fsingle cinfo 
    ContinuousSingleArrangement _ _ _ -> fcont cinfo 

type CanvasInfoMap = M.IntMap CanvasInfoBox

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

defaultPenWCS :: WidthColorStyle                                           
defaultPenWCS = WidthColorStyle predefined_medium ColorBlack               

defaultEraserWCS :: WidthColorStyle
defaultEraserWCS = WidthColorStyle predefined_eraser_medium ColorWhite

defaultTextWCS :: WidthColorStyle
defaultTextWCS = defaultPenWCS

defaultHighligherWCS :: WidthColorStyle
defaultHighligherWCS = WidthColorStyle predefined_highlighter_medium ColorYellow


defaultPenInfo :: PenInfo 
defaultPenInfo = 
  PenInfo { _penType = PenWork 
          , _penSet = PenHighlighterEraserSet { _currPen = defaultPenWCS
                                              , _currHighlighter = defaultHighligherWCS
                                              , _currEraser = defaultEraserWCS
                                              , _currText = defaultTextWCS }
          } 
                                           
$(mkLabels [''PenDraw, ''ViewInfo, ''PenInfo, ''PenHighlighterEraserSet, ''WidthColorStyle ])

