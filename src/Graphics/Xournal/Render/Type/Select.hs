{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Xournal.Render.Type.Select 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Graphics.Xournal.Render.Type.Select where

import Data.Xournal.Simple 
import Data.Xournal.Generic
import Data.Xournal.BBox
import Data.Xournal.Map
import Data.IntMap hiding (map)

import Graphics.Xournal.Render.Type.Hitted 

type TLayerSelect a = GLayer TEitherAlterHitted (StrokeTypeFromLayer a) 

type TLayerSelectBuf a = GLayerBuf (BufTypeFromLayer a) TEitherAlterHitted (StrokeTypeFromLayer a) 

type family StrokeTypeFromLayer a  :: * 
     
type family BufTypeFromLayer a :: *
     
type instance BufTypeFromLayer (GLayerBuf b s a) = b     
     
type instance StrokeTypeFromLayer TLayerBBox = StrokeBBox

data TLayerSelectInPage s a = TLayerSelectInPage 
                              { gselectedlayer :: TLayerSelect a 
                              , gotherlayers :: s a
                              }

data TLayerSelectInPageBuf s a = TLayerSelectInPageBuf
                               { gselectedlayerbuf :: TLayerSelectBuf a 
                               , gotherlayersbuf :: s a
                               }


-- | deprecated 

type TTempPageSelect = GPage Background (TLayerSelectInPage []) TLayerBBox
                       

-- | deprecated 

type TTempXournalSelect = GSelect (IntMap TPageBBoxMap) (Maybe (Int, TTempPageSelect))
