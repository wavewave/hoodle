{-# LANGUAGE TypeFamilies, TypeOperators, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.Type.Item 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
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
import           Control.Monad 
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import           Data.ByteString.Base64 
import           Graphics.GD.ByteString
import           Graphics.Rendering.Cairo
import qualified Graphics.Rendering.Cairo.SVG as RSVG
import           System.Directory
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
        src = img_src img
        embed = getByteStringIfEmbeddedPNG src 
    case embed of         
      Just bstr -> do 
        sfc <- saveTempPNGToCreateSurface bstr 
        return (RItemImage imgbbx (Just sfc))
      Nothing -> do
        let filesrc = C8.unpack (img_src img)
            filesrcext = takeExtension filesrc 
            imgaction 
              | filesrcext == ".PNG" || filesrcext == ".png" = 
                  Just <$> imageSurfaceCreateFromPNG filesrc
              | filesrcext == ".JPG" || filesrcext == ".jpg" = 
                  Just <$> getJPGandCreateSurface filesrc 
              | otherwise = return Nothing 
        msfc <- imgaction
        return (RItemImage imgbbx msfc)
cnstrctRItem (ItemSVG svg@(SVG _ _ bstr _ _)) = do 
    let str = C8.unpack bstr 
        svgbbx = mkSVGBBox svg
    rsvg <- RSVG.svgNewFromString str
    return (RItemSVG svgbbx (Just rsvg))

-- | get embedded png image. If not, just give me nothing. 
getByteStringIfEmbeddedPNG :: C8.ByteString -> Maybe C8.ByteString 
getByteStringIfEmbeddedPNG bstr = do 
    guard (C8.length bstr > 22)
    let (header,dat) = C8.splitAt 22 bstr 
    guard (header == "data:image/png;base64,") 
    either (const Nothing) return (decode dat)


-- | read JPG file using GD library and create cairo image surface
--   currently, this uses temporary png file (which is potentially dangerous)
getJPGandCreateSurface :: FilePath -> IO Surface 
getJPGandCreateSurface fp = do 
    img <- loadJpegFile fp  
    bstr <- savePngByteString img
    saveTempPNGToCreateSurface bstr 

-- | 
saveTempPNGToCreateSurface :: C8.ByteString -> IO Surface
saveTempPNGToCreateSurface bstr = do 
    tdir <- getTemporaryDirectory 
    let tfile = tdir </> "temp.png"
    B.writeFile tfile bstr 
    imageSurfaceCreateFromPNG tfile 
