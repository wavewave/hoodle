{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Foldable (for_)
import Data.GI.Base (AttrOp ((:=)), new, on)
import Data.Traversable (for)
import GI.Cairo.Render qualified as R
import GI.Cairo.Render.Connector as RC
import GI.Gtk qualified as Gtk
import GI.Pango qualified as P
import GI.PangoCairo qualified as PC

xs :: [Double]
xs = [0, 128 .. 1024]

ys :: [Double]
ys = [0, 128 .. 1024]

myText :: (P.Context, P.FontDescription, P.FontDescription) -> R.Render ()
myText (pangoCtxt, descSans, descMono) = do
    mkText descSans (120, 120) "Grid cache testing"
    mkText descMono (120, 240) "Grid cache testing"
  where
    mkText desc (x, y) msg = do
      layout :: P.Layout <- P.layoutNew pangoCtxt
      #setSize desc (10 * P.SCALE)
      #setFontDescription layout (Just desc)
      #setText layout msg (-1)
      R.moveTo x y
      ctxt <- RC.getContext
      PC.showLayout ctxt layout

myDraw :: (P.Context, P.FontDescription, P.FontDescription) -> R.Render ()
myDraw (pangoCtxt, descSans, descMono) = do
  R.setSourceRGBA 0 0 0 1.0
  R.setLineWidth 0.1
  for_ xs $ \x -> do
    R.moveTo x 0
    R.lineTo x 1024
    R.stroke
  for_ ys $ \y -> do
    R.moveTo 0 y
    R.lineTo 1024 y
    R.stroke
  myText (pangoCtxt, descSans, descMono)

initFont :: IO (Maybe (P.Context, P.FontDescription, P.FontDescription))
initFont = do
  fontMap :: PC.FontMap <- PC.fontMapGetDefault
  pangoCtxt <- #createContext fontMap
  familySans <- #getFamily fontMap "FreeSans"
  mfaceSans <- #getFace familySans Nothing
  familyMono <- #getFamily fontMap "FreeMono"
  mfaceMono <- #getFace familyMono Nothing
  for ((,) <$> mfaceSans <*> mfaceMono) $ \(faceSans, faceMono) -> do
    descSans <- #describe faceSans
    descMono <- #describe faceMono
    pure (pangoCtxt, descSans, descMono)

main :: IO ()
main = do
  _ <- Gtk.init Nothing
  Just (pangoCtxt, descSans, descMono) <- initFont
  mainWindow <- new Gtk.Window [#type := Gtk.WindowTypeToplevel]
  drawingArea <- new Gtk.DrawingArea []
  _ <- drawingArea `on` #draw $
    RC.renderWithContext $ do
      myDraw (pangoCtxt, descSans, descMono)
      pure True
  layout <- do
    vbox <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 0]
    #packStart vbox drawingArea True True 0
    pure vbox
  _ <- mainWindow `on` #destroy $ Gtk.mainQuit
  #add mainWindow layout
  #setDefaultSize mainWindow 640 480
  #showAll mainWindow
  Gtk.main
