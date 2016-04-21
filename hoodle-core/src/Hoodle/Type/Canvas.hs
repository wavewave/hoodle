{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-} 
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Type.Canvas 
-- Copyright   : (c) 2011-2016 Ian-Woo Kim
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
, MyScrollWindow (..)
, PenHighlighterEraserSet
, PenInfo
, WidthColorStyle
-- * default constructor 
, defaultViewInfoSinglePage 
, defaultCvsInfoSinglePage
, defaultCanvasWidgets
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
, mDrawSurface
, scrolledWindow
, viewInfo
, currentPageNum 
-- , currentPage
, horizAdjustment
, vertAdjustment
, horizAdjConnId 
, vertAdjConnId
, adjustments 
, canvasWidgets
, notifiedItem
-- , panZoomWidgetPosition
, currentTool 
, penWidth
, penColor
, currPen
, currHighlighter
, currEraser
, currText
, currVerticalSpace
, penType
, penSet
, variableWidthPen
-- * for box
, xfrmCvsInfo
, xfrmViewInfo
, getDrawAreaFromBox
, unboxLens
, unboxBiAct
, unboxBiXform
, forBoth
, forBoth'
-- * others
, updateCanvasDimForSingle
, updateCanvasDimForContSingle
) where

import           Control.Applicative ((<*>),(<$>))
import           Control.Lens (Simple,Lens,view,set,lens)
import qualified Data.IntMap as M
import           Data.Sequence
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.UI.Gtk as Gtk
-- 
import           Data.Hoodle.Simple (Dimension(..))
import           Data.Hoodle.BBox
import           Data.Hoodle.Predefined 
import           Graphics.Hoodle.Render.Type.Item
import           Graphics.Hoodle.Render.Type.Renderer
--
import           Hoodle.Type.Enum 
import           Hoodle.Type.PageArrangement
import           Hoodle.Type.Widget
--


-- |
data PenDraw = PenDraw { _points :: Seq (Double,Double) } 
             deriving (Show)

-- | lens for zoomMode 
points :: Simple Lens PenDraw (Seq (Double,Double))
points = lens _points (\f a -> f { _points = a } )

-- | 
data ViewInfo (a :: ViewMode) = 
       ViewInfo { _zoomMode :: ZoomMode 
                , _pageArrangement :: PageArrangement a } 

xfrmViewInfo :: (PageArrangement a -> PageArrangement b) 
             -> ViewInfo a 
             -> ViewInfo b
xfrmViewInfo f ViewInfo {..} = 
    ViewInfo { _zoomMode = _zoomMode
             , _pageArrangement = f _pageArrangement }

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

data MyScrollWindow = MyScrollWindow { _scrollCanvas     :: Gtk.VBox
                                     , _scrollHScrollbar :: Gtk.HScrollbar
                                     , _scrollVScrollbar :: Gtk.VScrollbar
                                     } 

-- |
data CanvasInfo (a :: ViewMode) = 
       CanvasInfo { _canvasId :: CanvasId
                  , _drawArea :: Gtk.DrawingArea
                  , _mDrawSurface :: Maybe Cairo.Surface 
                  , _scrolledWindow :: MyScrollWindow -- Gtk.ScrolledWindow
                  , _viewInfo :: ViewInfo a
                  , _currentPageNum :: Int
                  , _horizAdjustment :: Gtk.Adjustment
                  , _vertAdjustment :: Gtk.Adjustment 
                  , _horizAdjConnId :: Maybe (Gtk.ConnectId Gtk.Adjustment)
                  , _vertAdjConnId :: Maybe (Gtk.ConnectId Gtk.Adjustment)
                  , _canvasWidgets :: CanvasWidgets
                  , _notifiedItem :: Maybe (PageNum,BBox,RItem) 
                  }

-- |     
xfrmCvsInfo :: (ViewInfo a -> ViewInfo b) 
            -> CanvasInfo a -> CanvasInfo b 
xfrmCvsInfo f CanvasInfo {..} = 
    CanvasInfo { _canvasId = _canvasId
               , _drawArea = _drawArea 
               , _mDrawSurface = _mDrawSurface
               , _scrolledWindow = _scrolledWindow
               , _viewInfo = f _viewInfo
               , _currentPageNum = _currentPageNum 
               , _horizAdjustment = _horizAdjustment
               , _vertAdjustment = _vertAdjustment
               , _horizAdjConnId = _horizAdjConnId
               , _vertAdjConnId = _vertAdjConnId 
               , _canvasWidgets = _canvasWidgets
               , _notifiedItem = _notifiedItem
               }

-- |     
defaultCvsInfoSinglePage :: CanvasInfo 'SinglePage
defaultCvsInfoSinglePage = 
  CanvasInfo { _canvasId = error "defaultCvsInfoSinglePage cvsid"
             , _drawArea = error "defaultCvsInfoSinglePage DrawingArea"
             , _mDrawSurface = Nothing 
             , _scrolledWindow = error "ScrolledWindow"
             , _viewInfo = defaultViewInfoSinglePage
             , _currentPageNum = 0 
             , _horizAdjustment = error "adjustment"
             , _vertAdjustment = error "vadjust"
             , _horizAdjConnId = Nothing
             , _vertAdjConnId =  Nothing
             , _canvasWidgets = defaultCanvasWidgets
             , _notifiedItem = Nothing 
             }

-- | 
canvasId :: Simple Lens (CanvasInfo a) CanvasId 
canvasId = lens _canvasId (\f a -> f { _canvasId = a })

-- | 
drawArea :: Simple Lens (CanvasInfo a) Gtk.DrawingArea
drawArea = lens _drawArea (\f a -> f { _drawArea = a })

-- | 
mDrawSurface :: Simple Lens (CanvasInfo a) (Maybe Cairo.Surface) 
mDrawSurface = lens _mDrawSurface (\f a -> f { _mDrawSurface = a })


-- | 
scrolledWindow :: Simple Lens (CanvasInfo a) MyScrollWindow
scrolledWindow = lens _scrolledWindow (\f a -> f { _scrolledWindow = a })

-- |
viewInfo :: Simple Lens (CanvasInfo a) (ViewInfo a)
viewInfo = lens _viewInfo (\f a -> f { _viewInfo = a }) 

-- | 
currentPageNum :: Simple Lens (CanvasInfo a) Int 
currentPageNum = lens _currentPageNum (\f a -> f { _currentPageNum = a })

-- | 
horizAdjustment :: Simple Lens (CanvasInfo a) Gtk.Adjustment 
horizAdjustment = lens _horizAdjustment (\f a -> f { _horizAdjustment = a })

-- | 
vertAdjustment :: Simple Lens (CanvasInfo a) Gtk.Adjustment 
vertAdjustment = lens _vertAdjustment (\f a -> f { _vertAdjustment = a })

-- | ConnectId for horizontal scrollbar value change event 
horizAdjConnId :: Simple Lens (CanvasInfo a) (Maybe (Gtk.ConnectId Gtk.Adjustment))
horizAdjConnId = lens _horizAdjConnId (\f a -> f { _horizAdjConnId = a })

-- | ConnectId for vertical scrollbar value change event 
vertAdjConnId :: Simple Lens (CanvasInfo a) (Maybe (Gtk.ConnectId Gtk.Adjustment))
vertAdjConnId = lens _vertAdjConnId (\f a -> f { _vertAdjConnId = a })

-- | composition lens
adjustments :: Simple Lens (CanvasInfo a) (Gtk.Adjustment,Gtk.Adjustment) 
adjustments = lens getter setter  
  where getter = (,) <$> view horizAdjustment <*> view vertAdjustment 
        setter f (h,v) = set horizAdjustment h . set vertAdjustment v $ f
  
-- | lens for canavs widgets
canvasWidgets :: Simple Lens (CanvasInfo a) CanvasWidgets 
canvasWidgets = lens _canvasWidgets (\f a -> f { _canvasWidgets = a } )

-- | lens for notified item
notifiedItem :: Simple Lens (CanvasInfo a) (Maybe (PageNum,BBox,RItem))
notifiedItem = lens _notifiedItem (\f a -> f { _notifiedItem = a })

-- |
data CanvasInfoBox where 
  CanvasSinglePage :: CanvasInfo SinglePage -> CanvasInfoBox  
  CanvasContPage :: CanvasInfo ContinuousPage -> CanvasInfoBox
                   
forBoth :: ((CanvasInfo SinglePage -> f (CanvasInfo SinglePage))
             -> (CanvasInfo ContinuousPage -> f (CanvasInfo ContinuousPage)) 
             -> (CanvasInfoBox -> f CanvasInfoBox))   
        -> (forall a. CanvasInfo a -> f (CanvasInfo a))
        -> CanvasInfoBox -> f CanvasInfoBox
forBoth m f = m f f 


forBoth' :: ((CanvasInfo SinglePage -> r)
              -> (CanvasInfo ContinuousPage -> r) 
              -> (CanvasInfoBox -> r) )
         -> (forall a. CanvasInfo a -> r)
         -> CanvasInfoBox -> r
forBoth' m f = m f f 

-- | single page action and continuous page act
unboxBiXform :: (Functor f) => 
                (CanvasInfo SinglePage -> f (CanvasInfo SinglePage)) 
             -> (CanvasInfo ContinuousPage -> f (CanvasInfo ContinuousPage)) 
             -> CanvasInfoBox -> f CanvasInfoBox 
unboxBiXform fsingle _fcont (CanvasSinglePage cinfo) = fmap CanvasSinglePage (fsingle cinfo)
unboxBiXform _fsingle fcont (CanvasContPage cinfo) = fmap CanvasContPage (fcont cinfo) 
    
-- | single page action and continuous page act
unboxBiAct :: (CanvasInfo SinglePage -> r) 
           -> (CanvasInfo ContinuousPage -> r) 
           -> CanvasInfoBox -> r
unboxBiAct fsingle _fcont (CanvasSinglePage cinfo) = fsingle cinfo
unboxBiAct _fsingle fcont (CanvasContPage cinfo) = fcont cinfo


-- | 
unboxGet :: (forall a. Simple Lens (CanvasInfo a) b) -> CanvasInfoBox -> b 
unboxGet f = forBoth' unboxBiAct (view f) 

-- | 
unboxSet :: (forall a. Simple Lens (CanvasInfo a) b) -> b -> CanvasInfoBox -> CanvasInfoBox
unboxSet l b (CanvasSinglePage a) = CanvasSinglePage (set l b a)
unboxSet l b (CanvasContPage a) = CanvasContPage (set l b a) 

unboxLens :: (forall a. Simple Lens (CanvasInfo a) b) -> Simple Lens CanvasInfoBox b
unboxLens l = lens (unboxGet l) (flip (unboxSet l)) 

-- |
getDrawAreaFromBox :: CanvasInfoBox -> Gtk.DrawingArea 
getDrawAreaFromBox = view (unboxLens drawArea)

-- |
type CanvasInfoMap = M.IntMap CanvasInfoBox

-- | 
data WidthColorStyle = WidthColorStyle { _penWidth :: Double
                                       , _penColor :: PenColor } 
                     | NoWidthColorStyle 
                     deriving (Show)
                       
-- | lens for penWidth
penWidth :: Simple Lens WidthColorStyle Double
penWidth = lens _penWidth (\f a -> f { _penWidth = a } )

-- | lens for penColor
penColor :: Simple Lens WidthColorStyle PenColor
penColor = lens _penColor (\f a -> f { _penColor = a } )

-- | 
data PenHighlighterEraserSet = PenHighlighterEraserSet 
                               { _currPen :: WidthColorStyle 
                               , _currHighlighter :: WidthColorStyle 
                               , _currEraser :: WidthColorStyle 
                               , _currText :: WidthColorStyle
                               , _currVerticalSpace :: WidthColorStyle 
                               }
                             deriving (Show) 

-- | lens for currPen
currPen :: Simple Lens PenHighlighterEraserSet WidthColorStyle
currPen = lens _currPen (\f a -> f { _currPen = a } )

-- | lens for currHighlighter
currHighlighter :: Simple Lens PenHighlighterEraserSet WidthColorStyle
currHighlighter = lens _currHighlighter (\f a -> f { _currHighlighter = a } )

-- | lens for currEraser
currEraser :: Simple Lens PenHighlighterEraserSet WidthColorStyle
currEraser = lens _currEraser (\f a -> f { _currEraser = a } )

-- | lens for currText
currText :: Simple Lens PenHighlighterEraserSet WidthColorStyle
currText = lens _currText (\f a -> f { _currText = a } )

-- | lens for currText
currVerticalSpace :: Simple Lens PenHighlighterEraserSet WidthColorStyle
currVerticalSpace = lens _currVerticalSpace 
                      (\f a -> f { _currVerticalSpace = a } )
                     
-- | 
data PenInfo = PenInfo { _penType :: PenType
                       , _penSet :: PenHighlighterEraserSet 
                       , _variableWidthPen :: Bool 
                       } 
             deriving (Show) 

-- | lens for penType
penType :: Simple Lens PenInfo PenType 
penType = lens _penType (\f a -> f { _penType = a } )

-- | lens for penSet
penSet :: Simple Lens PenInfo PenHighlighterEraserSet
penSet = lens _penSet (\f a -> f { _penSet = a } )

-- | lens for variableWidthPen
variableWidthPen :: Simple Lens PenInfo Bool
variableWidthPen = lens _variableWidthPen (\f a -> f { _variableWidthPen = a } )

-- | 
currentTool :: Simple Lens PenInfo WidthColorStyle 
currentTool = lens chooser setter
  where chooser pinfo = case _penType pinfo of
                          PenWork -> _currPen . _penSet $ pinfo
                          HighlighterWork -> _currHighlighter . _penSet $ pinfo
                          EraserWork -> _currEraser . _penSet $ pinfo
                          VerticalSpaceWork -> NoWidthColorStyle
        setter pinfo wcs = 
          let pset = _penSet pinfo
              psetnew = case _penType pinfo of 
                          PenWork -> pset { _currPen = wcs }
                          HighlighterWork -> pset { _currHighlighter = wcs }
                          EraserWork -> pset { _currEraser = wcs }
                          VerticalSpaceWork -> pset 
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
                                              , _currText = defaultTextWCS 
                                              , _currVerticalSpace = NoWidthColorStyle 
                                              }
          , _variableWidthPen = False
          } 
                                           
-- | 
updateCanvasDimForSingle :: CanvasDimension 
                            -> CanvasInfo SinglePage  
                            -> IO (CanvasInfo SinglePage)
updateCanvasDimForSingle cdim@(CanvasDimension (Dim w' h')) cinfo = do 
  let zmode = view (viewInfo.zoomMode) cinfo
      SingleArrangement _ pdim (ViewPortBBox bbox)
        = view (viewInfo.pageArrangement) cinfo
      (x,y) = bbox_upperleft bbox 
      (sinvx,sinvy) = getRatioPageCanvas zmode pdim cdim 
      nbbox = BBox (x,y) (x+w'/sinvx,y+h'/sinvy)
      arr' = SingleArrangement cdim pdim (ViewPortBBox nbbox)
  maybe (return ()) Cairo.surfaceFinish $ view mDrawSurface cinfo 
  msfc <- fmap Just $ do 
            sfc <- Cairo.createImageSurface 
                     Cairo.FormatARGB32 (floor w') (floor h')
            Cairo.renderWith sfc $ do 
              Cairo.setSourceRGBA 0.5 0.5 0.5 1 
              Cairo.rectangle 0 0 w' h' 
              Cairo.fill 
            return sfc 
  return $ (set (viewInfo.pageArrangement) arr' . set mDrawSurface msfc) cinfo
     
-- | 

updateCanvasDimForContSingle :: PageDimension 
                             -> CanvasDimension 
                             -> CanvasInfo ContinuousPage 
                             -> IO (CanvasInfo ContinuousPage) 
updateCanvasDimForContSingle pdim cdim@(CanvasDimension (Dim w' h')) cinfo = do 
  let zmode = view (viewInfo.zoomMode) cinfo
      ContinuousArrangement _ ddim  func (ViewPortBBox bbox) 
        = view (viewInfo.pageArrangement) cinfo
      (x,y) = bbox_upperleft bbox 
      (sinvx,sinvy) = getRatioPageCanvas zmode pdim cdim 
      nbbox = BBox (x,y) (x+w'/sinvx,y+h'/sinvy)
      arr' = ContinuousArrangement cdim ddim func (ViewPortBBox nbbox)
  maybe (return ()) Cairo.surfaceFinish $ view mDrawSurface cinfo 
  msfc <- fmap Just $ do 
            sfc <- Cairo.createImageSurface 
                     Cairo.FormatARGB32 (floor w') (floor h')
            Cairo.renderWith sfc $ do 
              Cairo.setSourceRGBA 0.5 0.5 0.5 1 
              Cairo.rectangle 0 0 w' h' 
              Cairo.fill 
            return sfc 
  return $ (set (viewInfo.pageArrangement) arr'.set mDrawSurface msfc) cinfo
     
