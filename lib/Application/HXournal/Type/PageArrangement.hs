{-# LANGUAGE EmptyDataDecls, GADTs, TypeOperators, GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.Type.PageArrangement
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Application.HXournal.Type.PageArrangement where

import Control.Category ((.))
import Data.Xournal.Simple (Dimension(..))
import Data.Xournal.Generic
import Data.Xournal.BBox
import Data.Label 
import Prelude hiding ((.),id)
-- import qualified Data.IntMap as M

import Application.HXournal.Type.Predefined 
import Application.HXournal.Type.Alias
import Application.HXournal.Util

data ZoomMode = Original | FitWidth | FitHeight | Zoom Double 
              deriving (Show,Eq)

class ViewMode a 

data SinglePage = SinglePage
data ContinuousSinglePage = ContinuousSinglePage

instance ViewMode SinglePage 
instance ViewMode ContinuousSinglePage

newtype PageNum = PageNum { unPageNum :: Int } 
                deriving (Eq,Show,Ord,Num)

newtype ScreenCoordinate = ScrCoord { unScrCoord :: (Double,Double) } 
                         deriving (Show)
newtype CanvasCoordinate = CvsCoord { unCvsCoord :: (Double,Double) }
                         deriving (Show)
newtype DesktopCoordinate = DeskCoord { unDeskCoord :: (Double,Double) } 
                          deriving (Show)
newtype PageCoordinate = PageCoord { unPageCoord :: (Double,Double) } 
                       deriving (Show)

newtype ScreenDimension = ScreenDimension { unScreenDimension :: Dimension } 
                        deriving (Show)
newtype CanvasDimension = CanvasDimension { unCanvasDimension :: Dimension }
                        deriving (Show)
newtype CanvasOrigin = CanvasOrigin { unCanvasOrigin :: (Double,Double) } 
                       deriving (Show)
newtype PageOrigin = PageOrigin { unPageOrigin :: (Double,Double) } 
                   deriving (Show)
newtype PageDimension = PageDimension { unPageDimension :: Dimension } 
                      deriving (Show)
newtype DesktopDimension = DesktopDimension { unDesktopDimension :: Dimension }
                         deriving (Show)
newtype ViewPortBBox = ViewPortBBox { unViewPortBBox :: BBox } 
                     deriving (Show)

                     
apply :: (BBox -> BBox) -> ViewPortBBox -> ViewPortBBox 
apply f (ViewPortBBox bbox1) = ViewPortBBox (f bbox1)
{-# INLINE apply #-}

-- | data structure for coordinate arrangement of pages in desktop coordinate

data PageArrangement a where
  SingleArrangement:: CanvasDimension 
                      -> PageDimension 
                      -> ViewPortBBox 
                      -> PageArrangement SinglePage 
  ContinuousSingleArrangement :: CanvasDimension  
                                 -> DesktopDimension 
                                 -> (PageNum -> Maybe PageOrigin) 
                                 -> ViewPortBBox -> PageArrangement ContinuousSinglePage

-- | 

pageFunction :: PageArrangement ContinuousSinglePage -> PageNum -> Maybe PageOrigin
pageFunction (ContinuousSingleArrangement _ _ pfunc _ ) = pfunc 

-- | 

pageArrEither :: PageArrangement a 
                 -> Either (PageArrangement SinglePage) (PageArrangement ContinuousSinglePage)
pageArrEither arr@(SingleArrangement _ _ _) = Left arr 
pageArrEither arr@(ContinuousSingleArrangement _ _ _ _) = Right arr 

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

makeContinuousSingleArrangement :: ZoomMode -> CanvasDimension 
                                   -> Xournal EditMode 
                                   -> (PageNum,PageCoordinate) 
                                   -> PageArrangement ContinuousSinglePage
makeContinuousSingleArrangement zmode cdim@(CanvasDimension (Dim cw ch)) 
                                xoj (pnum,PageCoord (xpos,ypos)) = 
  let PageOrigin (_,y0) = maybeError "makeContSingleArr" $ pageArrFuncContSingle xoj pnum 
      dim@(Dim pw ph) = get g_dimension . head . gToList . get g_pages $ xoj
      (sinvx,sinvy) = getRatioPageCanvas zmode (PageDimension dim) cdim 
      vport = ViewPortBBox (BBox (xpos,ypos+y0) (xpos+cw/sinvx,ypos+y0+ch/sinvy))
  in ContinuousSingleArrangement cdim (deskDimContSingle xoj) (pageArrFuncContSingle xoj) vport 

-- |

pageArrFuncContSingle :: Xournal EditMode -> PageNum -> Maybe PageOrigin 
pageArrFuncContSingle xoj pnum@(PageNum n)
  | n < 0 = Nothing 
  | n >= len = Nothing 
  | otherwise = Just (PageOrigin (0,ys !! n))
  where addf x y = x + y + predefinedPageSpacing
        pgs = gToList . get g_pages $ xoj 
        len = length pgs 
        ys = scanl addf 0 . map (dim_height.get g_dimension) $ pgs  
        

deskDimContSingle :: Xournal EditMode -> DesktopDimension 
deskDimContSingle xoj = 
  let plst = gToList . get g_pages $ xoj
      PageOrigin (_,h') = maybeError 
                         ("deskdimContSingle" ++ 
                          show (map (pageArrFuncContSingle xoj . PageNum) [0..5])) 
                         $ pageArrFuncContSingle xoj 
                             (PageNum . (\x->x-1) . length $ plst )
      Dim _ h2 = get g_dimension (last plst)
      h = h' + h2 
      w = maximum . map (dim_width.get g_dimension) $ plst
  in DesktopDimension (Dim w h)

-- lenses

pageDimension :: PageArrangement SinglePage :-> PageDimension
pageDimension = lens getter setter 
  where getter (SingleArrangement _ pdim _) = pdim
        setter pdim (SingleArrangement cdim _ vbbox) = SingleArrangement cdim pdim vbbox

canvasDimension :: PageArrangement a :-> CanvasDimension 
canvasDimension = lens getter setter 
  where 
    getter (SingleArrangement cdim _ _) = cdim
    getter (ContinuousSingleArrangement cdim _ _ _) = cdim
    setter cdim (SingleArrangement _ pdim vbbox) = SingleArrangement cdim pdim vbbox
    setter cdim (ContinuousSingleArrangement _ ddim pfunc vbbox) = 
      ContinuousSingleArrangement cdim ddim pfunc vbbox

    
viewPortBBox :: PageArrangement a :-> ViewPortBBox 
viewPortBBox = lens getter setter 
  where 
    getter (SingleArrangement _ _ vbbox) = vbbox 
    getter (ContinuousSingleArrangement _ _ _ vbbox) = vbbox 
    setter vbbox (SingleArrangement cdim pdim _) = SingleArrangement cdim pdim vbbox 
    setter vbbox (ContinuousSingleArrangement cdim ddim pfunc _) = 
      ContinuousSingleArrangement cdim ddim pfunc vbbox

desktopDimension :: PageArrangement a :-> DesktopDimension 
desktopDimension = lens getter (error "setter for desktopDimension is not defined")
  where getter (SingleArrangement _ (PageDimension dim) _) = DesktopDimension dim
        getter (ContinuousSingleArrangement _ ddim _ _) = ddim 





