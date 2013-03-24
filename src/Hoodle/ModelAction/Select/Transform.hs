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
import           Control.Lens (view,set)
import           Control.Monad.Identity (runIdentity)
import           Data.Strict.Tuple
-- from hoodle-platform
import           Data.Hoodle.Generic
import           Data.Hoodle.BBox
import           Data.Hoodle.Simple hiding (Page,Hoodle)
import           Graphics.Hoodle.Render.Type
import           Graphics.Hoodle.Render.Type.HitTest
-- from this package
import           Hoodle.Type.Alias
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
changeItemBy func (RItemLink lnk rsvg) = RItemLink (changeLinkBy func lnk) rsvg
    


-- | modify stroke using a function
changeStrokeBy :: ((Double,Double)->(Double,Double)) -> BBoxed Stroke -> BBoxed Stroke
changeStrokeBy func (BBoxed (Stroke t c w ds) _bbox) = 
  let change ( x :!: y )  = let (nx,ny) = func (x,y) 
                            in nx :!: ny
      newds = map change ds 
      nstrk = Stroke t c w newds 
  in runIdentity (makeBBoxed nstrk) 
--       nbbox = bboxFromStroke nstrk 
--   in  BBoxed nstrk nbbox
changeStrokeBy func (BBoxed (VWStroke t c ds) _bbox) = 
  let change (x,y,z) = let (nx,ny) = func (x,y) 
                       in (nx,ny,z)
      newds = map change ds 
      nstrk = VWStroke t c newds 
  in runIdentity (makeBBoxed nstrk)
--       nbbox = bboxFromStroke nstrk 
--   in  BBoxed nstrk nbbox

-- | 
changeImageBy :: ((Double,Double)->(Double,Double)) -> BBoxed Image -> BBoxed Image
changeImageBy func (BBoxed (Image bstr (x,y) (Dim w h)) _bbox) = 
  let (x1,y1) = func (x,y) 
      (x2,y2) = func (x+w,y+h)
      nimg = Image bstr (x1,y1) (Dim (x2-x1) (y2-y1))
  in runIdentity (makeBBoxed nimg)

-- | 
changeSVGBy :: ((Double,Double)->(Double,Double)) -> BBoxed SVG -> BBoxed SVG
changeSVGBy func (BBoxed (SVG t c bstr (x,y) (Dim w h)) _bbox) = 
  let (x1,y1) = func (x,y) 
      (x2,y2) = func (x+w,y+h)
      nsvg = SVG t c  bstr (x1,y1) (Dim (x2-x1) (y2-y1))
  in runIdentity (makeBBoxed nsvg)
     
-- | 
changeLinkBy :: ((Double,Double)->(Double,Double)) -> BBoxed Link -> BBoxed Link
changeLinkBy func (BBoxed (Link i typ loc t c bstr (x,y) (Dim w h)) _bbox) = 
  let (x1,y1) = func (x,y) 
      (x2,y2) = func (x+w,y+h)
      nlnk = Link i typ loc t c  bstr (x1,y1) (Dim (x2-x1) (y2-y1))
  in runIdentity (makeBBoxed nlnk)     
changeLinkBy func (BBoxed (LinkDocID i lid loc t c bstr (x,y) (Dim w h)) _bbox) = 
  let (x1,y1) = func (x,y) 
      (x2,y2) = func (x+w,y+h)
      nlnk = LinkDocID i lid loc t c  bstr (x1,y1) (Dim (x2-x1) (y2-y1))
  in runIdentity (makeBBoxed nlnk)          





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


