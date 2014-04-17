{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.Util 
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
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

import qualified Graphics.Rendering.Cairo as Cairo
-- 
import           Data.Hoodle.BBox

-- | 
clipBBox :: Maybe BBox -> Cairo.Render ()
clipBBox (Just (BBox (x1,y1) (x2,y2))) = do 
    Cairo.resetClip 
    Cairo.rectangle x1 y1 (x2-x1) (y2-y1)
    Cairo.clip
clipBBox Nothing = Cairo.resetClip 

-- | 
clearBBox :: Maybe BBox -> Cairo.Render ()        
clearBBox Nothing = return ()
clearBBox (Just (BBox (x1,y1) (x2,y2))) = do 
    Cairo.save
    Cairo.setSourceRGBA 0 0 0 0
    Cairo.setOperator Cairo.OperatorSource
    Cairo.rectangle x1 y1 (x2-x1) (y2-y1) 
    Cairo.fill
    Cairo.restore
