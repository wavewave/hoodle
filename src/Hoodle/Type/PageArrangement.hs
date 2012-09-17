{-# LANGUAGE EmptyDataDecls, GADTs, TypeOperators, GeneralizedNewtypeDeriving, NoMonoPatBinds #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Type.PageArrangement
-- Copyright   : (c) 2012 Ian-Woo Kim
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
import           Control.Category ((.))
import           Control.Lens
import           Data.Maybe (fromJust)
-- from hoodle-platform 
import Data.Hoodle.Simple (Dimension(..))
import Data.Hoodle.Generic
import Data.Hoodle.BBox
-- from this package
import Hoodle.Type.Predefined 
import Hoodle.Type.Alias
import Hoodle.Util
-- 
import Prelude hiding ((.),id)
import Debug.Trace

-- | 

data ZoomMode = Original | FitWidth | FitHeight | Zoom Double 
              deriving (Show,Eq)

-- | 

class ViewMode a 

-- | only one page show at a time
data SinglePage = SinglePage

-- | 
instance ViewMode SinglePage 

-- | continuously show pages in general
data ContinuousPage = ContinuousPage

-- | 
instance ViewMode ContinuousPage

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

-- | data structure for coordinate arrangement of pages in desktop coordinate
data PageArrangement a where
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
  let dim = view g_dimension . head . gToList . view g_pages $ hdl
      (sinvx,sinvy) = getRatioPageCanvas zmode (PageDimension dim) cdim 
      cnstrnt = DesktopWidthConstrained (cw/sinvx)     
      (PageOrigin (x0,y0),_) = maybeError "makeContArr" $ pageArrFuncCont cnstrnt hdl pnum 
      (x1,y1) = (0,0) -- for the time being (xpos+x0,ypos+y0)
      (x2,y2) = (cw/sinvx,ch/sinvy) -- for the time being (xpos+x0+cw/sinvx,ypos+y0+ch/sinvy)
      ddim@(DesktopDimension (Dim w h)) = deskDimCont cnstrnt hdl 
      (x1',x2') = if x2 > w && w-(x2-x1) > 0 then (w-(x2-x1),w) else (x1,x2)
      (y1',y2') = if y2 > h && h-(y2-y1) > 0 then (h-(y2-y1),h) else (y1,y2)
      vport = ViewPortBBox (BBox (x1',y1') (x2',y2') )
  in trace ("ddim = " ++ show ddim ++ 
            "\n(x0,y0) = " ++ show (x0,y0) ++
            "\n(xpos,ypos) = " ++ show (xpos,ypos) ++ 
            "\nvport = " ++ show vport ) 
     
     
     $  ContinuousArrangement cdim ddim (pageArrFuncCont cnstrnt hdl) vport 


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
        pgs = gToList . view g_pages $ hdl 
        len = length pgs 
        pdims = map (view g_dimension) pgs 
        wh2xyFrmPg = ((,) <$> dim_width <*> dim_height) . view g_dimension
        xys = scanl addf (0,0) . map wh2xyFrmPg $ pgs  

-- |
deskDimCont :: DesktopConstraint -> Hoodle EditMode -> DesktopDimension 
deskDimCont cnstrnt hdl = 
    let pgs = gToList . view g_pages $ hdl
        len = length pgs 
        olst = map (fromJust . pageArrFuncCont cnstrnt hdl . PageNum) [0..len-1] 
        -- dlst = map (view g_dimension) pgs
        -- odlst = zip olst dlst
        f (PageOrigin (x,y),PageDimension (Dim w h)) (Dim w' h') = 
          let w'' = if w' < x+w then x+w else w' 
              h'' = if h' < y+h then y+h else h' 
          in Dim w'' h''
    in DesktopDimension $ foldr f (Dim 0 0) olst

------------
-- lenses
------------

-- | 
pageDimension :: Simple Lens (PageArrangement SinglePage) PageDimension
pageDimension = lens getter setter 
  where getter (SingleArrangement _ pdim _) = pdim
        getter (ContinuousArrangement _ _ _ _) = error $ "in pageDimension " -- partial 
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



{-
-- | 
pageFunction :: PageArrangement ContinuousSinglePage -> PageNum -> Maybe PageOrigin
pageFunction (ContinuousSingleArrangement _ _ pfunc _ ) = pfunc 

-- | 
pageArrEither :: PageArrangement a 
                 -> Either (PageArrangement SinglePage) (PageArrangement ContinuousSinglePage)
pageArrEither arr@(SingleArrangement _ _ _) = Left arr 
pageArrEither arr@(ContinuousSingleArrangement _ _ _ _) = Right arr 

-}

      
{-      PageOrigin (w',h') = maybeError 
                         ("deskdimCont" ++ 
                          show (map (pageArrFuncCont cdim hdl . PageNum) [0..5])) 
                         $ pageArrFuncCont cdim hdl 
                             (PageNum . (\x->x-1) . length $ plst ) 
      Dim _ h2 = view g_dimension (last plst)
      h = h' + h2 
      w = maximum . map (dim_width.view g_dimension) $ plst
  in DesktopDimension (Dim w h) -}


{- --- previous version 
-- |
pageArrFuncCont :: Hoodle EditMode -> PageNum -> Maybe PageOrigin 
pageArrFuncCont hdl (PageNum n)
  | n < 0 = Nothing 
  | n >= len = Nothing 
  | otherwise = Just (PageOrigin (0,ys !! n))
  where addf x y = x + y + predefinedPageSpacing
        pgs = gToList . view g_pages $ hdl 
        len = length pgs 
        ys = scanl addf 0 . map (dim_height.view g_dimension) $ pgs  
-}
        
{- --- previous version 
-- |
deskDimCont :: Hoodle EditMode -> DesktopDimension 
deskDimCont hdl = 
  let plst = gToList . view g_pages $ hdl
      PageOrigin (_,h') = maybeError 
                         ("deskdimCont" ++ 
                          show (map (pageArrFuncCont hdl . PageNum) [0..5])) 
                         $ pageArrFuncCont hdl 
                             (PageNum . (\x->x-1) . length $ plst )
      Dim _ h2 = view g_dimension (last plst)
      h = h' + h2 
      w = maximum . map (dim_width.view g_dimension) $ plst
  in DesktopDimension (Dim w h)
-}


