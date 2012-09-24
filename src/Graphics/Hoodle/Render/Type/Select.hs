{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.Type.Select 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Graphics.Hoodle.Render.Type.Select where

-- from other packages
import Control.Compose
import Control.Lens 
import Data.IntMap hiding (map, fromList)
-- from hoodle-platform 
import Data.Hoodle.BBox
import Data.Hoodle.Generic
import Data.Hoodle.Simple 
import Data.Hoodle.Select
import Data.Hoodle.Zipper
-- import Data.Hoodle.Map
-- from this package
import Graphics.Hoodle.Render.Type.Hoodle 
import Graphics.Hoodle.Render.Type.Background
import Graphics.Hoodle.Render.Type.HitTest



----------------------------
-- select state rendering --
----------------------------

type TLayerSelectBuf a = GLayer (BufTypeFromLayer a) TEitherAlterHitted (StrokeTypeFromLayer a) 


type family StrokeTypeFromLayer a  :: * 
     
type family BufTypeFromLayer a :: *
     
type instance BufTypeFromLayer (GLayer b s a) = b     
     
type instance StrokeTypeFromLayer RLayer = StrokeBBox 

data TLayerSelectInPageBuf s a = TLayerSelectInPageBuf
                               { gselectedlayerbuf :: TLayerSelectBuf a 
                               , gotherlayersbuf :: s a
                               }


-- -- | 
-- newtype LyBuf = LyBuf { mbuffer :: Maybe Surface } 


-- -- | 
-- type TPageBBoxMapPDFBuf = 
--   TPageBBoxMapBkgBuf BackgroundPDFDrawable LyBuf
  
-- -- |   
-- type THoodleBBoxMapPDFBuf = 
--   THoodleBBoxMapBkgBuf BackgroundPDFDrawable LyBuf
  
-- |
type TTempPageSelectPDFBuf = 
  GPage RBackground (TLayerSelectInPageBuf ZipperSelect) RLayer

-- | 
type TTempHoodleSelectPDFBuf = 
  GSelect (IntMap RPage) (Maybe (Int, TTempPageSelectPDFBuf))


-- | 
hLayer2RLayer :: TLayerSelectBuf RLayer -> RLayer 
hLayer2RLayer l = 
  case unTEitherAlterHitted (view gstrokes l) of
    Left strs -> GLayer (view gbuffer l) strs 
    Right alist -> GLayer (view gbuffer l) . Prelude.concat 
                   $ interleave id unHitted alist

-- | 
hPage2RPage :: TTempPageSelectPDFBuf -> RPage
hPage2RPage p = 
  let TLayerSelectInPageBuf s others = view glayers p 
      s' = hLayer2RLayer s
      normalizedothers = case others of   
        NoSelect [] -> error "something wrong in hPage2RPage" 
        NoSelect (x:xs) -> Select (fromList (x:xs))
        Select (O (Nothing)) -> error "something wrong in hPage2RPage"
        Select (O (Just _)) -> others 
      Select (O (Just sz)) = normalizedothers 
  in GPage (view gdimension p) (view gbackground p) (Select . O . Just $ replace s' sz)





{-
-- | deprecated 

type TTempPageSelect = GPage Background (TLayerSelectInPage []) TLayerBBox
                       

-- | deprecated 

type TTempHoodleSelect = GSelect (IntMap TPageBBoxMap) (Maybe (Int, TTempPageSelect))

-}


-- type TLayerSelect a = GLayer TEitherAlterHitted (StrokeTypeFromLayer a) 
