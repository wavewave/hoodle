{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Graphics.Hoodle.Render.Item where

import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Identity (runIdentity)
import qualified Data.ByteString as B
import Data.ByteString.Base64 (decode)
import qualified Data.ByteString.Char8 as C8
import Data.Hoodle.BBox (makeBBoxed)
import Data.Hoodle.Simple
  ( Anchor (..),
    Item (..),
    Link (..),
    SVG (..),
  )
import Data.UUID.V4 (nextRandom)
import Graphics.GD.ByteString
  ( loadJpegFile,
    savePngByteString,
  )
import Graphics.Hoodle.Render.Type.Item (RItem (..))
import Graphics.Hoodle.Render.Type.Renderer (Renderer)
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Cairo.SVG as RSVG
import Hoodle.Util.Process (checkPipe)
import System.Directory
  ( getTemporaryDirectory,
    removeFile,
  )
import System.FilePath ((<.>), (</>))

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
    then
      let lnkbbx = runIdentity (makeBBoxed lnk)
       in return (RItemLink lnkbbx Nothing)
    else do
      let str = C8.unpack bstr
          lnkbbx = runIdentity (makeBBoxed lnk)
      rsvg <- liftIO $ RSVG.svgNewFromString str
      return (RItemLink lnkbbx (Just rsvg))
cnstrctRItem (ItemAnchor anc@(Anchor _ bstr _ _)) =
  if C8.null bstr
    then
      let ancbbx = runIdentity (makeBBoxed anc)
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
  let (header, dat) = C8.splitAt 22 bstr
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
