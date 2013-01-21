{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.ModelAction.Select.Transform 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.ModelAction.Select.Transform where

-- from other package
import           Control.Category
import           Control.Lens
import           Control.Monad
import           Data.Algorithm.Diff
import           Data.Foldable (foldl')
import qualified Data.IntMap as M
import qualified Data.Map as Map 
import           Data.Monoid
import           Data.Sequence (ViewL(..),viewl,Seq)
import           Data.Strict.Tuple
import           Data.Time.Clock
import           Graphics.Rendering.Cairo
import           Graphics.Rendering.Cairo.Matrix ( invert, transformPoint )
import           Graphics.UI.Gtk hiding (get,set)
-- from hoodle-platform
import           Data.Hoodle.Generic
import           Data.Hoodle.BBox
import           Data.Hoodle.Select 
import           Data.Hoodle.Simple hiding (Page,Hoodle)
import           Graphics.Hoodle.Render
import           Graphics.Hoodle.Render.Type
import           Graphics.Hoodle.Render.Type.HitTest
import           Graphics.Hoodle.Render.Util 
import           Graphics.Hoodle.Render.Util.HitTest
-- from this package
import           Hoodle.ModelAction.Layer
import           Hoodle.Type.Alias
import           Hoodle.Type.Enum
import           Hoodle.Type.HoodleState
import           Hoodle.Type.Predefined 
import           Hoodle.Type.PageArrangement
import           Hoodle.Util
import           Hoodle.View.Coordinate
-- 
import Prelude hiding ((.),id)

-- |
rItmsInActiveLyr :: Page SelectMode -> Either [RItem] (TAlterHitted RItem)
rItmsInActiveLyr = unTEitherAlterHitted.view (glayers.selectedLayer.gitems)

-- |    
changeItemBy :: ((Double,Double)->(Double,Double)) -> RItem -> RItem
changeItemBy func (RItemStroke strk) = RItemStroke (changeStrokeBy func strk)
changeItemBy func (RItemImage img sfc) = RItemImage (changeImageBy func img) sfc
changeItemBy func (RItemSVG svg rsvg) = RItemSVG (changeSVGBy func svg) rsvg
    


-- | modify stroke using a function
changeStrokeBy :: ((Double,Double)->(Double,Double)) -> StrokeBBox -> StrokeBBox
changeStrokeBy func (StrokeBBox (Stroke t c w ds) _bbox) = 
  let change ( x :!: y )  = let (nx,ny) = func (x,y) 
                            in nx :!: ny
      newds = map change ds 
      nstrk = Stroke t c w newds 
      nbbox = bboxFromStroke nstrk 
  in  StrokeBBox nstrk nbbox
changeStrokeBy func (StrokeBBox (VWStroke t c ds) _bbox) = 
  let change (x,y,z) = let (nx,ny) = func (x,y) 
                       in (nx,ny,z)
      newds = map change ds 
      nstrk = VWStroke t c newds 
      nbbox = bboxFromStroke nstrk 
  in  StrokeBBox nstrk nbbox

-- | 
changeImageBy :: ((Double,Double)->(Double,Double)) -> ImageBBox -> ImageBBox
changeImageBy func (ImageBBox (Image bstr (x,y) (Dim w h)) _bbox) = 
  let (x1,y1) = func (x,y) 
      (x2,y2) = func (x+w,y+h)
      nimg = Image bstr (x1,y1) (Dim (x2-x1) (y2-y1))
  in mkImageBBox nimg 

-- | 
changeSVGBy :: ((Double,Double)->(Double,Double)) -> SVGBBox -> SVGBBox
changeSVGBy func (SVGBBox (SVG t c bstr (x,y) (Dim w h)) _bbox) = 
  let (x1,y1) = func (x,y) 
      (x2,y2) = func (x+w,y+h)
      nsvg = SVG t c  bstr (x1,y1) (Dim (x2-x1) (y2-y1))
  in mkSVGBBox nsvg 


-- | modify the whole selection using a function
changeSelectionBy :: ((Double,Double) -> (Double,Double))
                     -> Page SelectMode -> Page SelectMode
changeSelectionBy func tpage = 
  let activelayer = rItmsInActiveLyr tpage
      buf = view (glayers.selectedLayer.gbuffer) tpage
  in case activelayer of 
       Left _ -> tpage 
       Right alist -> 
         let alist' =fmapAL id 
                            (Hitted . map (changeItemBy func) . unHitted) 
                            alist 
             layer' = GLayer buf . TEitherAlterHitted . Right $ alist'
         in set (glayers.selectedLayer) layer' tpage 

   

-- | special case of offset modification
changeSelectionByOffset :: (Double,Double) -> Page SelectMode -> Page SelectMode
changeSelectionByOffset (offx,offy) = changeSelectionBy (offsetFunc (offx,offy))

-- |
offsetFunc :: (Double,Double) -> (Double,Double) -> (Double,Double) 
offsetFunc (offx,offy) = \(x,y)->(x+offx,y+offy)


