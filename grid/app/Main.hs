{-# LANGUAGE OverloadedLabels #-}

module Main where

import Data.Foldable (for_)
import Data.GI.Base (AttrOp ((:=)), new, on)
import qualified GI.Cairo.Render as R
import GI.Cairo.Render.Connector (renderWithContext)
import qualified GI.Gtk as Gtk

xs :: [Double]
xs = [0, 128 .. 1024]

ys :: [Double]
ys = [0, 128 .. 1024]


myDraw :: R.Render ()
myDraw = do
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

main :: IO ()
main = do
  _ <- Gtk.init Nothing
  mainWindow <- new Gtk.Window [#type := Gtk.WindowTypeToplevel]
  drawingArea <- new Gtk.DrawingArea []
  _ <- drawingArea `on` #draw $
    renderWithContext $ myDraw >> pure True
  layout <- do
    vbox <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 0]
    #packStart vbox drawingArea True True 0
    pure vbox
  _ <- mainWindow `on` #destroy $ Gtk.mainQuit
  #add mainWindow layout
  #setDefaultSize mainWindow 640 480
  #showAll mainWindow
  Gtk.main
