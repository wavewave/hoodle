{-# OPTIONS_GHC -w #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Data.GI.Base.ManagedPtr (withManagedPtr)
import qualified GI.Cairo.Render as R
import GI.Cairo.Render.Connector (renderWithContext)
import qualified GI.Gtk as GI


main :: IO ()
main = do
  _ <- GI.init Nothing
  mainWindow <- GI.windowNew GI.WindowTypeToplevel
  drawingArea <- GI.drawingAreaNew
  GI.onWidgetDraw drawingArea $
    renderWithContext $ do
      R.setSourceRGBA 0.16 0.18 0.19 1.0
      R.setLineWidth (1.5/60)
      R.rectangle 10 10 50 50
      R.fill
      -- withManagedPtr fp $ \p -> do
      --let cairo = Cairo (castPtr p)
      --renderWith $ \sfc -> do
      liftIO $ putStrLn "draw called"
      pure True
  layout <- do
    vbox <- GI.boxNew GI.OrientationVertical 0
    GI.boxPackStart vbox drawingArea True True 0
    pure vbox
  GI.containerAdd mainWindow layout
  GI.widgetShowAll mainWindow
  GI.main
