{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.Item 
-- Copyright   : (c) 2011-2015 Ian-Woo Kim
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

import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.IO.Class
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import           Data.ByteString.Base64
import           Data.UUID.V4
import           Graphics.GD.ByteString
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Cairo.SVG as RSVG
import           System.Directory
import           System.FilePath
-- from hoodle-platform 
import           Data.Hoodle.BBox
import           Data.Hoodle.Simple
import           Hoodle.Util.Process
-- from this package
import           Graphics.Hoodle.Render.Type.Item
import           Graphics.Hoodle.Render.Type.Renderer

-- | construct renderable item 
cnstrctRItem :: Item -> Renderer RItem 
cnstrctRItem (ItemStroke strk) = return (RItemStroke (runIdentity (makeBBoxed strk)))
cnstrctRItem (ItemImage img) = 
    let imgbbx = runIdentity (makeBBoxed img)
    in return (RItemImage imgbbx Nothing)
cnstrctRItem (ItemSVG svg@(SVG _ _ bstr _ _)) = do 
    let str = C8.unpack bstr 
        svgbbx = runIdentity (makeBBoxed svg)
    rsvg <- liftIO (RSVG.svgNewFromString str)
    return (RItemSVG svgbbx (Just rsvg))
cnstrctRItem (ItemLink lnk@(Link _ _ _ _ _ bstr _ _)) = do 
    let str = C8.unpack bstr 
        lnkbbx = runIdentity (makeBBoxed lnk)
    rsvg <- liftIO $ RSVG.svgNewFromString str
    return (RItemLink lnkbbx (Just rsvg))
cnstrctRItem (ItemLink lnk@(LinkDocID _ _ _ _ _ bstr _ _)) = do 
    let str = C8.unpack bstr 
        lnkbbx = runIdentity (makeBBoxed lnk)
    rsvg <- liftIO $ RSVG.svgNewFromString str
    return (RItemLink lnkbbx (Just rsvg))    
cnstrctRItem (ItemLink lnk@(LinkAnchor _ _ _ _ bstr _ _)) = 
    if C8.null bstr 
    then let lnkbbx = runIdentity (makeBBoxed lnk) 
         in return (RItemLink lnkbbx Nothing) 
    else do 
      let str = C8.unpack bstr 
          lnkbbx = runIdentity (makeBBoxed lnk)
      rsvg <- liftIO $ RSVG.svgNewFromString str
      return (RItemLink lnkbbx (Just rsvg))    
cnstrctRItem (ItemAnchor anc@(Anchor _ bstr _ _)) = 
    if C8.null bstr 
    then let ancbbx = runIdentity (makeBBoxed anc) 
         in return (RItemAnchor ancbbx Nothing)
    else do 
      let str = C8.unpack bstr 
          ancbbx = runIdentity (makeBBoxed anc)
      rsvg <- liftIO $ RSVG.svgNewFromString str
      return (RItemAnchor ancbbx (Just rsvg))    


-- | get embedded png image. If not, just give me nothing. 
getByteStringIfEmbeddedPNG :: C8.ByteString -> Maybe C8.ByteString 
getByteStringIfEmbeddedPNG bstr = do 
    guard (C8.length bstr > 22)
    let (header,dat) = C8.splitAt 22 bstr 
    guard (header == "data:image/png;base64,") 
    either (const Nothing) return (decode dat)


-- | read JPG file using GD library and create cairo image surface
--   currently, this uses temporary png file (which is potentially dangerous)
getJPGandCreateSurface :: FilePath -> IO Cairo.Surface 
getJPGandCreateSurface fp = do 
    img <- loadJpegFile fp  
    bstr <- savePngByteString img
    saveTempPNGToCreateSurface bstr 

-- | 
saveTempPNGToCreateSurface :: C8.ByteString -> IO Cairo.Surface
saveTempPNGToCreateSurface bstr = do 
    tdir <- getTemporaryDirectory
    tuuid <- nextRandom
    let tfile = tdir </> show tuuid <.> "png"
    B.writeFile tfile bstr
    checkPipe tfile
    sfc <- Cairo.imageSurfaceCreateFromPNG tfile
    removeFile tfile
    return sfc

