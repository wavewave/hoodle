{-# LANGUAGE EmptyDataDecls #-} 
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-} 
{-# LANGUAGE NoMonoPatBinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Type.PageArrangement
-- Copyright   : (c) 2012-2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Type.PageArrangement where

-- from other packages
import           Control.Applicative
import           Control.Lens (Simple,Lens,view,lens)
import           Data.Foldable (toList)
-- from hoodle-platform 
import Data.Hoodle.Simple (Dimension(..))
import Data.Hoodle.Generic
import Data.Hoodle.BBox
-- from this package
import Hoodle.Type.Predefined 
import Hoodle.Type.Alias
import Hoodle.Util
-- 

-- | supported zoom modes
data ZoomMode = Original | FitWidth | FitHeight | Zoom Double 
              deriving (Show,Eq)

-- | supported view modes
data ViewMode = SinglePage | ContinuousPage 

-- | 
newtype PageNum = PageNum { unPageNum :: Int } 
                deriving (Eq,Show,Ord,Num)

-- |
newtype ScreenCoordinate = ScrCoord { unScrCoord :: (Double,Double) } 
                         deriving (Show)
                                  
-- |                                   
newtype CanvasCoordinate = CvsCoord { unCvsCoord :: (Double,Double) }
                         deriving (Show)
                                  
-- |                                   
newtype DesktopCoordinate = DeskCoord { unDeskCoord :: (Double,Double) } 
                          deriving (Show)
                                   
-- | 
newtype PageCoordinate = PageCoord { unPageCoord :: (Double,Double) } 
                       deriving (Show)

-- | 
newtype ScreenDimension = ScreenDimension { unScreenDimension :: Dimension } 
                        deriving (Show)
                                 
-- |                                  
newtype CanvasDimension = CanvasDimension { unCanvasDimension :: Dimension }
                        deriving (Show)
                                 
-- |                                  
newtype CanvasOrigin = CanvasOrigin { unCanvasOrigin :: (Double,Double) } 
                       deriving (Show)
                                
-- |                                 
newtype PageOrigin = PageOrigin { unPageOrigin :: (Double,Double) } 
                   deriving (Show)
                            
-- |                             
newtype PageDimension = PageDimension { unPageDimension :: Dimension } 
                      deriving (Show)
                               
-- |                               
newtype DesktopDimension = DesktopDimension { unDesktopDimension :: Dimension }
                         deriving (Show)
                                  
-- |                                   
newtype ViewPortBBox = ViewPortBBox { unViewPortBBox :: BBox } 
                     deriving (Show)

-- |                      
apply :: (BBox -> BBox) -> ViewPortBBox -> ViewPortBBox 
apply f (ViewPortBBox bbox1) = ViewPortBBox (f bbox1)
{-# INLINE apply #-}


-- | 
xformViewPortFitInSize :: Dimension -> (BBox -> BBox) -> ViewPortBBox -> ViewPortBBox
xformViewPortFitInSize (Dim w h) f (ViewPortBBox bbx) = 
  let BBox (x1,y1) (x2,y2) = f bbx 
      xmargin = if 0.5*((x2-x1)-w) > 0 then 0.5*((x2-x1)-w) else 0
      ymargin = if 0.5*((y2-y1)-h) > 0 then 0.5*((y2-y1)-h) else 0 
      (x1',x2') 
        | x2>w && w-(x2-x1)>0  = (w-(x2-x1),w) 
        | x2>w && w-(x2-x1)<=0 = (-xmargin,-xmargin+x2-x1)
        | x1< (-xmargin) = (-xmargin,-xmargin+x2-x1)
        | otherwise            = (x1,x2)
      (y1',y2') 
        | y2>h && h-(y2-y1)>0  = (h-(y2-y1),h)
        | y2>h && h-(y2-y1)<=0 = (-ymargin,-ymargin+y2-y1) 
        | y1 < (-ymargin) =  (-ymargin,-ymargin+y2-y1) 
        | otherwise            = (y1,y2)
  in ViewPortBBox (BBox (x1',y1') (x2',y2') )
      
      
      

-- | data structure for coordinate arrangement of pages in desktop coordinate
data PageArrangement (a :: ViewMode) where
  SingleArrangement :: CanvasDimension 
                    -> PageDimension 
                    -> ViewPortBBox 
                    -> PageArrangement SinglePage 
  ContinuousArrangement :: CanvasDimension  
                        -> DesktopDimension 
                        -> (PageNum -> Maybe (PageOrigin,PageDimension)) 
                        -> ViewPortBBox 
                        -> PageArrangement ContinuousPage


-- |
getRatioPageCanvas :: ZoomMode -> PageDimension -> CanvasDimension -> (Double,Double)
getRatioPageCanvas zmode (PageDimension (Dim w h)) (CanvasDimension (Dim w' h')) = 
  case zmode of 
    Original -> (1.0,1.0)
    FitWidth -> (w'/w,w'/w)
    FitHeight -> (h'/h,h'/h)
    Zoom s -> (s,s)

-- |
makeSingleArrangement :: ZoomMode 
                         -> PageDimension 
                         -> CanvasDimension 
                         -> (Double,Double) 
                         -> PageArrangement SinglePage
makeSingleArrangement zmode pdim cdim@(CanvasDimension (Dim w' h')) (x,y) = 
  let (sinvx,sinvy) = getRatioPageCanvas zmode pdim cdim
      bbox = BBox (x,y) (x+w'/sinvx,y+h'/sinvy)
  in SingleArrangement cdim pdim (ViewPortBBox bbox) 

-- | 
data DesktopConstraint = DesktopWidthConstrained Double 

-- | 
makeContinuousArrangement :: ZoomMode 
                          -> CanvasDimension 
                          -> Hoodle EditMode 
                          -> (PageNum,PageCoordinate) 
                          -> PageArrangement ContinuousPage
makeContinuousArrangement zmode cdim@(CanvasDimension (Dim cw ch)) 
                                hdl (pnum,PageCoord (xpos,ypos)) = 
  let dim = view gdimension . head . toList . view gpages $ hdl
      (sinvx,sinvy) = getRatioPageCanvas zmode (PageDimension dim) cdim 
      cnstrnt = DesktopWidthConstrained (cw/sinvx)     
      -- default to zero if error 
      (PageOrigin (x0,y0),_) = maybe (PageOrigin (0,0),PageDimension (Dim cw ch)) 
                                     id (pageArrFuncCont cnstrnt hdl pnum)
      ddim@(DesktopDimension iddim) = deskDimCont cnstrnt hdl 
      (x1,y1) = (xpos+x0,ypos+y0) 
      (x2,y2) = (xpos+x0+cw/sinvx,ypos+y0+ch/sinvy) 
      ovport =ViewPortBBox (BBox (x1,y1) (x2,y2))
      vport = xformViewPortFitInSize iddim id ovport
  in ContinuousArrangement cdim ddim (pageArrFuncCont cnstrnt hdl) vport


-- |
pageArrFuncCont :: DesktopConstraint
                -> Hoodle EditMode 
                -> PageNum 
                -> Maybe (PageOrigin,PageDimension) 
pageArrFuncCont (DesktopWidthConstrained w') hdl (PageNum n)
  | n < 0 = Nothing
  | n >= len = Nothing
  | otherwise = Just (PageOrigin (xys !! n), PageDimension (pdims !! n))
  where addf (x,y) (w,h) = if x+2*w+predefinedPageSpacing < w'   
                             then (x+w+predefinedPageSpacing,y)
                             else (0,y+h+predefinedPageSpacing)
        pgs = toList . view gpages $ hdl 
        len = length pgs 
        pdims = map (view gdimension) pgs 
        wh2xyFrmPg = ((,) <$> dim_width <*> dim_height) . view gdimension
        xys = scanl addf (0,0) . map wh2xyFrmPg $ pgs  

-- |
deskDimCont :: DesktopConstraint -> Hoodle EditMode -> DesktopDimension 
deskDimCont cnstrnt hdl =  
    let pgs = toList . view gpages $ hdl
        len = length pgs 
        olst = maybeError' "deskDimCont" $
          mapM (pageArrFuncCont cnstrnt hdl . PageNum) [0..len-1]
        f (PageOrigin (x,y),PageDimension (Dim w h)) (Dim w' h') = 
          let w'' = if w' < x+w then x+w else w' 
              h'' = if h' < y+h then y+h else h' 
          in Dim w'' h''
    in DesktopDimension $ foldr f (Dim 0 0) olst

------------
-- lenses
------------

-- | 
pageDimension :: Simple Lens (PageArrangement a) PageDimension
pageDimension = lens getter setter 
  where 
    getter :: PageArrangement a -> PageDimension
    getter (SingleArrangement _ pdim _) = pdim
    getter (ContinuousArrangement _ _ _ _) = error $ "in pageDimension " -- partial 
 
    setter :: PageArrangement a -> PageDimension -> PageArrangement a
    setter (SingleArrangement cdim _ vbbox) pdim = SingleArrangement cdim pdim vbbox
    setter (ContinuousArrangement _ _ _ _) _pdim = error $ "in pageDimension "  -- partial 
        
-- | 
canvasDimension :: Simple Lens (PageArrangement a) CanvasDimension 
canvasDimension = lens getter setter 
  where 
    getter :: PageArrangement a -> CanvasDimension
    getter (SingleArrangement cdim _ _) = cdim
    getter (ContinuousArrangement cdim _ _ _) = cdim
    setter :: PageArrangement a -> CanvasDimension -> PageArrangement a
    setter (SingleArrangement _ pdim vbbox) cdim = SingleArrangement cdim pdim vbbox
    setter (ContinuousArrangement _ ddim pfunc vbbox) cdim = 
      ContinuousArrangement cdim ddim pfunc vbbox

-- |    
viewPortBBox :: Simple Lens (PageArrangement a) ViewPortBBox 
viewPortBBox = lens getter setter 
  where 
    getter :: PageArrangement a -> ViewPortBBox   
    getter (SingleArrangement _ _ vbbox) = vbbox 
    getter (ContinuousArrangement _ _ _ vbbox) = vbbox 
    setter :: PageArrangement a -> ViewPortBBox -> PageArrangement a
    setter (SingleArrangement cdim pdim _) vbbox = SingleArrangement cdim pdim vbbox 
    setter (ContinuousArrangement cdim ddim pfunc _) vbbox = 
      ContinuousArrangement cdim ddim pfunc vbbox

-- | 
desktopDimension :: Simple Lens (PageArrangement a) DesktopDimension 
desktopDimension = lens getter (error "setter for desktopDimension is not defined")
  where getter :: PageArrangement a -> DesktopDimension
        getter (SingleArrangement _ (PageDimension dim) _) = DesktopDimension dim
        getter (ContinuousArrangement _ ddim _ _) = ddim 



