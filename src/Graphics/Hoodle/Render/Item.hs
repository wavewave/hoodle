{-# LANGUAGE TypeFamilies, TypeOperators, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.Type.Item 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Rendering RItem 
-- 
-----------------------------------------------------------------------------

module Graphics.Hoodle.Render.Item where

import Graphics.Rendering.Cairo
-- from hoodle-platform 
import Data.Hoodle.BBox 
import Data.Hoodle.Simple
-- from this package
import Graphics.Hoodle.Render.Type.Item 

-- | construct renderable item 
cnstrctRItem :: Item -> IO RItem 
cnstrctRItem (ItemStroke strk) = return (RItemStroke (mkStrokeBBox strk))
cnstrctRItem (ItemImage img) = do 
    let imgbbx = mkImageBBox img
        bbx = imgbbx_bbx imgbbx
        Dim w h = bboxToDim bbx
    sfc <- createImageSurface FormatARGB32 (floor w) (floor h) 
    -- rendering is not implemented yet
    return (RItemImage imgbbx (Just sfc))
