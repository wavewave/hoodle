{-# LANGUAGE TemplateHaskell, TypeOperators, ExistentialQuantification,
             Rank2Types, GADTs #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Type.Canvas 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Type.Canvas 
( 
-- * data types 
  CanvasId
, PenDraw (..)
, emptyPenDraw
, ViewInfo (..)
, CanvasInfo (..) 
, CanvasInfoBox (..)
, CanvasInfoMap
-- , PenType (..) 
, WidthColorStyle
, PenHighlighterEraserSet
, PenInfo
-- * default constructor 
, defaultViewInfoSinglePage 
, defaultCvsInfoSinglePage
, defaultPenWCS
, defaultEraserWCS
, defaultTextWCS
, defaultHighligherWCS
, defaultPenInfo
-- * lenses
, points 
, zoomMode 
, pageArrangement 
, canvasId
, drawArea 
, scrolledWindow
, viewInfo
, currentPageNum 
-- , currentPage
, horizAdjustment
, vertAdjustment
, horizAdjConnId 
, vertAdjConnId
, adjustments 
, currentTool 
, penWidth
, penColor
, currPen
, currHighlighter
, currEraser
, currText
, penType
, penSet
, variableWidthPen
-- * for box
, getDrawAreaFromBox
, unboxGet
, fmapBox 
, boxAction
, selectBoxAction
, selectBox
, pageArrEitherFromCanvasInfoBox
, viewModeBranch
-- * others
-- , getPage
, updateCanvasDimForSingle
, updateCanvasDimForContSingle
) where

import           Control.Applicative ((<*>),(<$>))
import           Control.Category
import           Control.Lens
import           Control.Monad.Identity (Identity(..))
import qualified Data.IntMap as M
import           Data.Sequence
import           Graphics.UI.Gtk hiding (get,set)
-- 
import           Data.Hoodle.Simple (Dimension(..))
import           Data.Hoodle.BBox
import           Data.Hoodle.Predefined 
--
import           Hoodle.Type.Enum 
import           Hoodle.Type.PageArrangement
--
import Prelude hiding ((.), id)

-- |
type CanvasId = Int 

-- |
data PenDraw = PenDraw { _points :: Seq (Double,Double) } 
             deriving (Show)
                      
-- | 
data ViewInfo a  = (ViewMode a) => 
                   ViewInfo { _zoomMode :: ZoomMode 
                            , _pageArrangement :: PageArrangement a } 


-- | 
emptyPenDraw :: PenDraw
emptyPenDraw = PenDraw empty


-- | default view info with single page mode

defaultViewInfoSinglePage :: ViewInfo SinglePage
defaultViewInfoSinglePage = 
  ViewInfo { _zoomMode = Original 
           , _pageArrangement = 
             SingleArrangement (CanvasDimension (Dim 100 100))
                               (PageDimension (Dim 100 100)) 
                               (ViewPortBBox (BBox (0,0) (100,100))) } 

-- | lens for zoomMode 
zoomMode :: Simple Lens (ViewInfo a) ZoomMode 
zoomMode = lens _zoomMode (\f a -> f { _zoomMode = a } )

-- | 
pageArrangement :: Simple Lens (ViewInfo a) (PageArrangement a)
pageArrangement = lens _pageArrangement (\f a -> f { _pageArrangement = a })

-- |
data CanvasInfo a = 
    (ViewMode a) => CanvasInfo { _canvasId :: CanvasId
                               , _drawArea :: DrawingArea
                               , _scrolledWindow :: ScrolledWindow
                               , _viewInfo :: ViewInfo a
                               , _currentPageNum :: Int
                               , _horizAdjustment :: Adjustment
                               , _vertAdjustment :: Adjustment 
                               , _horizAdjConnId :: Maybe (ConnectId Adjustment)
                               , _vertAdjConnId :: Maybe (ConnectId Adjustment)
                               }
    
defaultCvsInfoSinglePage :: CanvasInfo SinglePage
defaultCvsInfoSinglePage = 
  CanvasInfo { _canvasId = error "cvsid"
             , _drawArea = error "DrawingArea"
             , _scrolledWindow = error "ScrolledWindow"
             , _viewInfo = defaultViewInfoSinglePage
             , _currentPageNum = 0 
             , _horizAdjustment = error "adjustment"
             , _vertAdjustment = error "vadjust"
             , _horizAdjConnId = Nothing
             , _vertAdjConnId =  Nothing
             }

-- | 
canvasId :: Simple Lens (CanvasInfo a) CanvasId 
canvasId = lens _canvasId (\f a -> f { _canvasId = a })

-- | 
drawArea :: Simple Lens (CanvasInfo a) DrawingArea
drawArea = lens _drawArea (\f a -> f { _drawArea = a })

-- | 
scrolledWindow :: Simple Lens (CanvasInfo a) ScrolledWindow
scrolledWindow = lens _scrolledWindow (\f a -> f { _scrolledWindow = a })

-- |
viewInfo :: Simple Lens (CanvasInfo a) (ViewInfo a)
viewInfo = lens _viewInfo (\f a -> f { _viewInfo = a }) 

-- | 
currentPageNum :: Simple Lens (CanvasInfo a) Int 
currentPageNum = lens _currentPageNum (\f a -> f { _currentPageNum = a })

-- | 
horizAdjustment :: Simple Lens (CanvasInfo a) Adjustment 
horizAdjustment = lens _horizAdjustment (\f a -> f { _horizAdjustment = a })

-- | 
vertAdjustment :: Simple Lens (CanvasInfo a) Adjustment 
vertAdjustment = lens _vertAdjustment (\f a -> f { _vertAdjustment = a })

-- | ConnectId for horizontal scrollbar value change event 
horizAdjConnId :: Simple Lens (CanvasInfo a) (Maybe (ConnectId Adjustment))
horizAdjConnId = lens _horizAdjConnId (\f a -> f { _horizAdjConnId = a })

-- | ConnectId for vertical scrollbar value change event 
vertAdjConnId :: Simple Lens (CanvasInfo a) (Maybe (ConnectId Adjustment))
vertAdjConnId = lens _vertAdjConnId (\f a -> f { _vertAdjConnId = a })

-- | composition lens
adjustments :: Simple Lens (CanvasInfo a) (Adjustment,Adjustment) 
adjustments = lens getter setter  
  where getter = (,) <$> view horizAdjustment <*> view vertAdjustment 
        setter f (h,v) = set horizAdjustment h . set vertAdjustment v $ f
  
  {- Lens $ (,) <$> (fst `for` horizAdjustment)
                         <*> (snd `for` vertAdjustment) -}

-- |
data CanvasInfoBox = forall a. (ViewMode a) => CanvasInfoBox (CanvasInfo a) 

-- |
getDrawAreaFromBox :: CanvasInfoBox -> DrawingArea 
getDrawAreaFromBox = unboxGet drawArea

-- | 
unboxGet :: (forall a. (ViewMode a) => Simple Lens (CanvasInfo a) b) -> CanvasInfoBox -> b 
unboxGet f (CanvasInfoBox x) = view f x

-- | fmap-like operation for box
fmapBox :: (forall a. (ViewMode a) => CanvasInfo a -> CanvasInfo a)
        -> CanvasInfoBox -> CanvasInfoBox
fmapBox f (CanvasInfoBox cinfo) = CanvasInfoBox (f cinfo)

-- | 
boxAction :: Monad m => (forall a. ViewMode a => CanvasInfo a -> m b) 
             -> CanvasInfoBox -> m b 
boxAction f (CanvasInfoBox cinfo) = f cinfo 

-- | either-like operation
selectBoxAction :: (Monad m) => 
                   (CanvasInfo SinglePage -> m a) 
                -> (CanvasInfo ContinuousSinglePage -> m a) -> CanvasInfoBox -> m a 
selectBoxAction fsingle fcont (CanvasInfoBox cinfo) = 
  case view (viewInfo.pageArrangement) cinfo of 
    SingleArrangement _ _ _ ->  fsingle cinfo 
    ContinuousSingleArrangement _ _ _ _ -> fcont cinfo 

-- |     
selectBox :: (CanvasInfo SinglePage -> CanvasInfo SinglePage)
          -> (CanvasInfo ContinuousSinglePage -> CanvasInfo ContinuousSinglePage)
          -> CanvasInfoBox -> CanvasInfoBox 
selectBox fsingle fcont = 
  let idaction :: CanvasInfoBox -> Identity CanvasInfoBox
      idaction = selectBoxAction (return . CanvasInfoBox . fsingle) (return . CanvasInfoBox . fcont)
  in runIdentity . idaction   

-- | 
pageArrEitherFromCanvasInfoBox :: CanvasInfoBox 
                 -> Either (PageArrangement SinglePage) (PageArrangement ContinuousSinglePage)
pageArrEitherFromCanvasInfoBox (CanvasInfoBox cinfo) = 
  pageArrEither . view (viewInfo.pageArrangement) $ cinfo 

-- | 
viewModeBranch :: (CanvasInfo SinglePage -> CanvasInfo SinglePage) 
               -> (CanvasInfo ContinuousSinglePage -> CanvasInfo ContinuousSinglePage) 
               -> CanvasInfo v -> CanvasInfo v 
viewModeBranch fsingle fcont cinfo = 
  case view (viewInfo.pageArrangement) cinfo of 
    SingleArrangement _ _ _ ->  fsingle cinfo 
    ContinuousSingleArrangement _ _ _ _ -> fcont cinfo 

-- |
type CanvasInfoMap = M.IntMap CanvasInfoBox

-- | 
data WidthColorStyle = WidthColorStyle { _penWidth :: Double
                                       , _penColor :: PenColor } 
                     deriving (Show)
                       
-- | 
data PenHighlighterEraserSet = PenHighlighterEraserSet 
                               { _currPen :: WidthColorStyle 
                               , _currHighlighter :: WidthColorStyle 
                               , _currEraser :: WidthColorStyle 
                               , _currText :: WidthColorStyle}
                             deriving (Show) 
                     
-- | 
data PenInfo = PenInfo { _penType :: PenType
                       , _penSet :: PenHighlighterEraserSet 
                       , _variableWidthPen :: Bool 
                       } 
             deriving (Show) 

-- | 
currentTool :: Simple Lens PenInfo WidthColorStyle 
currentTool = lens chooser setter
  where chooser pinfo = case _penType pinfo of
                          PenWork -> _currPen . _penSet $ pinfo
                          HighlighterWork -> _currHighlighter . _penSet $ pinfo
                          EraserWork -> _currEraser . _penSet $ pinfo
                          TextWork -> _currText . _penSet $ pinfo 
        setter pinfo wcs = 
          let pset = _penSet pinfo
              psetnew = case _penType pinfo of 
                          PenWork -> pset { _currPen = wcs }
                          HighlighterWork -> pset { _currHighlighter = wcs }
                          EraserWork -> pset { _currEraser = wcs }
                          TextWork -> pset { _currText = wcs }
          in  pinfo { _penSet = psetnew } 

-- |         
defaultPenWCS :: WidthColorStyle  
defaultPenWCS = WidthColorStyle predefined_medium ColorBlack               

-- | 
defaultEraserWCS :: WidthColorStyle
defaultEraserWCS = WidthColorStyle predefined_eraser_medium ColorWhite

-- | 
defaultTextWCS :: WidthColorStyle
defaultTextWCS = defaultPenWCS

-- | 
defaultHighligherWCS :: WidthColorStyle
defaultHighligherWCS = WidthColorStyle predefined_highlighter_medium ColorYellow

-- | 
defaultPenInfo :: PenInfo 
defaultPenInfo = 
  PenInfo { _penType = PenWork 
          , _penSet = PenHighlighterEraserSet { _currPen = defaultPenWCS
                                              , _currHighlighter = defaultHighligherWCS
                                              , _currEraser = defaultEraserWCS
                                              , _currText = defaultTextWCS }
          , _variableWidthPen = False
          } 
                                           
makeLenses ''PenDraw
makeLenses ''ViewInfo
makeLenses ''PenInfo
makeLenses ''PenHighlighterEraserSet
makeLenses ''WidthColorStyle 

-- | 
updateCanvasDimForSingle :: CanvasDimension 
                            -> CanvasInfo SinglePage  
                            -> CanvasInfo SinglePage 
updateCanvasDimForSingle cdim@(CanvasDimension (Dim w' h')) cinfo = 
  let zmode = view (viewInfo.zoomMode) cinfo
      SingleArrangement _ pdim (ViewPortBBox bbox)
        = view (viewInfo.pageArrangement) cinfo
      (x,y) = bbox_upperleft bbox 
      (sinvx,sinvy) = getRatioPageCanvas zmode pdim cdim 
      nbbox = BBox (x,y) (x+w'/sinvx,y+h'/sinvy)
      arr' = SingleArrangement cdim pdim (ViewPortBBox nbbox)
  in set (viewInfo.pageArrangement) arr' cinfo
     
-- | 

updateCanvasDimForContSingle :: PageDimension 
                                -> CanvasDimension 
                                -> CanvasInfo ContinuousSinglePage 
                                -> CanvasInfo ContinuousSinglePage 
updateCanvasDimForContSingle pdim cdim@(CanvasDimension (Dim w' h')) cinfo = 
  let zmode = view (viewInfo.zoomMode) cinfo
      ContinuousSingleArrangement _ ddim  func (ViewPortBBox bbox) 
        = view (viewInfo.pageArrangement) cinfo
      (x,y) = bbox_upperleft bbox 
      (sinvx,sinvy) = getRatioPageCanvas zmode pdim cdim 
      nbbox = BBox (x,y) (x+w'/sinvx,y+h'/sinvy)
      arr' = ContinuousSingleArrangement cdim ddim func (ViewPortBBox nbbox)
  in set (viewInfo.pageArrangement) arr' cinfo
     
