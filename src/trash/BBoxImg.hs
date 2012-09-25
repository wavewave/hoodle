{-# LANGUAGE TypeFamilies, StandaloneDeriving, RecordWildCards, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Hoodle.BBoxImg
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Data.Hoodle.BBoxImg where

import Data.Hoodle.BBox
import Data.Hoodle.Generic
import Data.Hoodle.Simple
import Graphics.Rendering.Cairo 
-- 
import Prelude hiding (fst,snd)
import qualified Prelude as Prelude (fst,snd)



-- | 
data StrokeBBoxImg = NoImage StrokeBBox 
                   | Image StrokeBBox (Maybe Surface)
                   
instance GCast StrokeBBoxImg StrokeBBox where
  gcast (NoImage str) = str 
  gcast (Image str _) = str 
  
instance GCast StrokeBBox StrokeBBoxImg where 
  gcast s@(StrokeBBox (Stroke _ _ _ _) _) = NoImage s 
  gcast s@(StrokeBBox (VWStroke _ _ _) _) = NoImage s 
  gcast s@(StrokeBBox (Img _ _ _) _) = Image s Nothing 


type TLayerBBoxImg = GLayer [] StrokeBBoxImg

type TPageBBoxImg = GPage Background [] TLayerBBoxImg

type THoodleBBoxImg = GHoodle [] TPageBBoxImg


instance GStrokeable StrokeBBoxImg where  
  gFromStroke = gcast . mkStrokeBBoxFromStroke
  gToStroke = strokeFromStrokeBBox . gcast 

