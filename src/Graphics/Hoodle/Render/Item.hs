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

import           Control.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import           Graphics.GD.ByteString
import           Graphics.Rendering.Cairo
import qualified Graphics.Rendering.Cairo.SVG as RSVG
import           System.Directory
import           System.Environment 
import           System.FilePath 
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
        filesrc = C8.unpack (img_src img)
        filesrcext = takeExtension filesrc 
        imgaction 
          | filesrcext == "PNG" || filesrcext == "png" = 
              Just <$> imageSurfaceCreateFromPNG filesrc
          | filesrcext == "JPG" || filesrcext == "jpg" = 
              Just <$> getJPGandCreateSurface filesrc 
          | otherwise = return Nothing 
    msfc <- imgaction
    -- rendering is not implemented yet
    return (RItemImage imgbbx msfc)
cnstrctRItem (ItemSVG svg@(SVG _ _ bstr (x,y) _)) = do 
    let str = C8.unpack bstr 
        svgbbx = mkSVGBBox svg
    rsvg <- RSVG.svgNewFromString str
    return (RItemSVG svgbbx (Just rsvg))


-- | read JPG file using GD library and create cairo image surface
--   currently, this uses temporary png file (which is potentially dangerous)
getJPGandCreateSurface :: FilePath -> IO Surface 
getJPGandCreateSurface fp = do 
    img <- loadJpegFile fp  
    bstr <- savePngByteString img
    tdir <- getTemporaryDirectory 
    let tfile = tdir </> "temp.png"
    B.writeFile tfile bstr 
    imageSurfaceCreateFromPNG tfile 

