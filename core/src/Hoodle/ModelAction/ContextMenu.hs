{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hoodle.ModelAction.ContextMenu where

import qualified Data.ByteString.Char8 as B
import Data.Hoodle.BBox (BBox (..))
import Data.Hoodle.Simple (Dimension (..), SVG (..))
import Data.UUID.V4 (nextRandom)
import Graphics.Hoodle.Render (renderRItem)
import Graphics.Hoodle.Render.Type
  ( CanvasId,
    RItem,
    RenderCache,
    isLinkInRItem,
  )
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.UI.Gtk as Gtk
import Hoodle.Type.Event
  ( AllEvent (UsrEv),
    ContextMenuEvent (..),
    UserEvent (GotContextMenuSignal, OpenLink),
  )
import Hoodle.Util (UrlPath (FileUrl, HttpUrl))
import System.Directory
  ( getTemporaryDirectory,
    removeFile,
  )
import System.FilePath ((<.>), (</>))

-- |
menuOpenALink :: (AllEvent -> IO ()) -> UrlPath -> IO Gtk.MenuItem
menuOpenALink evhandler urlpath = do
  let urlname = case urlpath of
        FileUrl fp -> fp
        HttpUrl url -> url
  menuitemlnk <- Gtk.menuItemNewWithLabel ("Open " ++ urlname :: String)
  _ <- menuitemlnk `Gtk.on` Gtk.menuItemActivate $ evhandler (UsrEv (OpenLink urlpath Nothing))
  return menuitemlnk

-- |
menuCreateALink :: (AllEvent -> IO ()) -> [RItem] -> IO (Maybe Gtk.MenuItem)
menuCreateALink evhandler sitems =
  if not (any isLinkInRItem sitems)
    then do
      mi <- Gtk.menuItemNewWithLabel ("Create a link to..." :: String)
      _ <-
        mi `Gtk.on` Gtk.menuItemActivate $
          evhandler (UsrEv (GotContextMenuSignal CMenuCreateALink))
      return (Just mi)
    else return Nothing

-- |
makeSVGFromSelection :: RenderCache -> CanvasId -> [RItem] -> BBox -> IO SVG
makeSVGFromSelection cache cid hititms (BBox (ulx, uly) (lrx, lry)) = do
  uuid <- nextRandom
  tdir <- getTemporaryDirectory
  let filename = tdir </> show uuid <.> "svg"
      (x, y) = (ulx, uly)
      (w, h) = (lrx - ulx, lry - uly)
  Cairo.withSVGSurface filename w h $ \s -> Cairo.renderWith s $ do
    Cairo.translate (-ulx) (-uly)
    mapM_ (renderRItem cache cid) hititms
  bstr <- B.readFile filename
  let svg = SVG Nothing Nothing bstr (x, y) (Dim w h)
  svg `seq` removeFile filename
  return svg
