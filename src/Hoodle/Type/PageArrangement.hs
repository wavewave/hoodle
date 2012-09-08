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
import Control.Category ((.))
import           Control.Lens
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

-- | 

data ZoomMode = Original | FitWidth | FitHeight | Zoom Double 
              deriving (Show,Eq)

-- | 

class ViewMode a 

-- |

data SinglePage = SinglePage

-- | 

data ContinuousSinglePage = ContinuousSinglePage

-- | 
instance ViewMode SinglePage 
instance ViewMode ContinuousSinglePage

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
                                   -> Hoodle EditMode 
                                   -> (PageNum,PageCoordinate) 
                                   -> PageArrangement ContinuousSinglePage
makeContinuousSingleArrangement zmode cdim@(CanvasDimension (Dim cw ch)) 
                                hdl (pnum,PageCoord (xpos,ypos)) = 
  let PageOrigin (_,y0) = maybeError "makeContSingleArr" $ pageArrFuncContSingle hdl pnum 
      dim = view g_dimension . head . gToList . view g_pages $ hdl
      (sinvx,sinvy) = getRatioPageCanvas zmode (PageDimension dim) cdim 
      (x1,y1) = (xpos,ypos+y0)
      (x2,y2) = (xpos+cw/sinvx,ypos+y0+ch/sinvy)
      ddim@(DesktopDimension (Dim w h)) = deskDimContSingle hdl 
      (x1',x2') = if x2 > w && w-(x2-x1) > 0 then (w-(x2-x1),w) else (x1,x2)
      (y1',y2') = if y2 > h && h-(y2-y1) > 0 then (h-(y2-y1),h) else (y1,y2)
      vport = ViewPortBBox (BBox (x1',y1') (x2',y2') )
  in ContinuousSingleArrangement cdim ddim (pageArrFuncContSingle hdl) vport 

-- |

pageArrFuncContSingle :: Hoodle EditMode -> PageNum -> Maybe PageOrigin 
pageArrFuncContSingle hdl (PageNum n)
  | n < 0 = Nothing 
  | n >= len = Nothing 
  | otherwise = Just (PageOrigin (0,ys !! n))
  where addf x y = x + y + predefinedPageSpacing
        pgs = gToList . view g_pages $ hdl 
        len = length pgs 
        ys = scanl addf 0 . map (dim_height.view g_dimension) $ pgs  
        

deskDimContSingle :: Hoodle EditMode -> DesktopDimension 
deskDimContSingle hdl = 
  let plst = gToList . view g_pages $ hdl
      PageOrigin (_,h') = maybeError 
                         ("deskdimContSingle" ++ 
                          show (map (pageArrFuncContSingle hdl . PageNum) [0..5])) 
                         $ pageArrFuncContSingle hdl 
                             (PageNum . (\x->x-1) . length $ plst )
      Dim _ h2 = view g_dimension (last plst)
      h = h' + h2 
      w = maximum . map (dim_width.view g_dimension) $ plst
  in DesktopDimension (Dim w h)

-- lenses

pageDimension :: Simple Lens (PageArrangement SinglePage) PageDimension
pageDimension = lens getter setter 
  where getter (SingleArrangement _ pdim _) = pdim
        getter (ContinuousSingleArrangement _ _ _ _) = error $ "in pageDimension " -- partial 
        setter (SingleArrangement cdim _ vbbox) pdim = SingleArrangement cdim pdim vbbox
        setter (ContinuousSingleArrangement _ _ _ _) _pdim = error $ "in pageDimension "  -- partial 
        

canvasDimension :: Simple Lens (PageArrangement a) CanvasDimension 
canvasDimension = lens getter setter 
  where 
    getter :: PageArrangement a -> CanvasDimension
    getter (SingleArrangement cdim _ _) = cdim
    getter (ContinuousSingleArrangement cdim _ _ _) = cdim
    setter :: PageArrangement a -> CanvasDimension -> PageArrangement a
    setter (SingleArrangement _ pdim vbbox) cdim = SingleArrangement cdim pdim vbbox
    setter (ContinuousSingleArrangement _ ddim pfunc vbbox) cdim = 
      ContinuousSingleArrangement cdim ddim pfunc vbbox

    
viewPortBBox :: Simple Lens (PageArrangement a) ViewPortBBox 
viewPortBBox = lens getter setter 
  where 
    getter :: PageArrangement a -> ViewPortBBox   
    getter (SingleArrangement _ _ vbbox) = vbbox 
    getter (ContinuousSingleArrangement _ _ _ vbbox) = vbbox 
    setter :: PageArrangement a -> ViewPortBBox -> PageArrangement a
    setter (SingleArrangement cdim pdim _) vbbox = SingleArrangement cdim pdim vbbox 
    setter (ContinuousSingleArrangement cdim ddim pfunc _) vbbox = 
      ContinuousSingleArrangement cdim ddim pfunc vbbox

desktopDimension :: Simple Lens (PageArrangement a) DesktopDimension 
desktopDimension = lens getter (error "setter for desktopDimension is not defined")
  where getter :: PageArrangement a -> DesktopDimension
        getter (SingleArrangement _ (PageDimension dim) _) = DesktopDimension dim
        getter (ContinuousSingleArrangement _ ddim _ _) = ddim 





