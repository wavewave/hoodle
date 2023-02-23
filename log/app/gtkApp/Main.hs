{-# OPTIONS_GHC -w #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.GI.Base (new, on, AttrOp ((:=)))
import Data.GI.Base.ManagedPtr (withManagedPtr)
import qualified GI.Cairo.Render as R
import GI.Cairo.Render.Connector (renderWithContext)
import qualified GI.Gtk as GI

main :: IO ()
main = do
  _ <- GI.init Nothing
  mainWindow <- new GI.Window [ #type := GI.WindowTypeToplevel ]
  drawingArea <- new GI.DrawingArea []
  drawingArea `on` #draw $
    renderWithContext $ do
      R.setSourceRGBA 0.16 0.18 0.19 1.0
      R.setLineWidth (1.5/60)
      R.rectangle 10 10 50 50
      R.fill
      liftIO $ putStrLn "draw called"
      pure True
  layout <- do
    vbox <- new GI.Box [#orientation := GI.OrientationVertical, #spacing := 0]
    #packStart vbox drawingArea True True 0
    pure vbox
  #add mainWindow layout
  #showAll mainWindow
  GI.main
