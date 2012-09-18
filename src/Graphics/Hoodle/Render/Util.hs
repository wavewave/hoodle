{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.Util 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- utility 
--
-----------------------------------------------------------------------------

module Graphics.Hoodle.Render.Util where

import           Graphics.Rendering.Cairo
-- 
import           Data.Hoodle.BBox

-- | 
clipBBox :: Maybe BBox -> Render ()
clipBBox (Just (BBox (x1,y1) (x2,y2))) = do 
    resetClip 
    rectangle x1 y1 (x2-x1) (y2-y1)
    clip
clipBBox Nothing = resetClip 

-- | 
clearBBox :: Maybe BBox -> Render ()        
clearBBox Nothing = return ()
clearBBox (Just (BBox (x1,y1) (x2,y2))) = do 
    save
    setSourceRGBA 0 0 0 0
    setOperator OperatorSource
    rectangle x1 y1 (x2-x1) (y2-y1) 
    fill
    restore
