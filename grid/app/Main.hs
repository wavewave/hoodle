{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Foldable (for_)
import Data.GI.Base (AttrOp ((:=)), new, on)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Traversable (for)
import GI.Cairo.Render qualified as R
import GI.Cairo.Render.Connector as RC
import GI.Gtk qualified as Gtk
import GI.Pango qualified as P
import GI.PangoCairo qualified as PC

loremIpsum :: Text
loremIpsum =
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit, \n\
  \sed do eiusmod tempor incididunt ut labore et dolore magn\n\
  \a aliqua. Ut enim ad minim veniam, quis nostrud exercitat\n\
  \ion ullamco laboris nisi ut aliquip ex ea commodo consequ\n\
  \at. Duis aute irure dolor in reprehenderit in voluptate v\n\
  \elit esse cillum dolore eu fugiat nulla pariatur. Excepte\n\
  \ur sint occaecat cupidatat non proident, sunt in culpa qu\n\
  \i officia deserunt mollit anim id est laborum."

xs :: [Double]
xs = [0, 128 .. 1024]

ys :: [Double]
ys = [0, 128 .. 1024]

drawTextLine :: (P.Context, P.FontDescription) -> (Double, Double) -> Text -> R.Render ()
drawTextLine (pctxt, desc) (x, y) msg = do
  layout :: P.Layout <- P.layoutNew pctxt
  #setSize desc (10 * P.SCALE)
  #setFontDescription layout (Just desc)
  #setText layout msg (-1)
  R.moveTo x y
  ctxt <- RC.getContext
  PC.showLayout ctxt layout

drawTextMultiline ::
  (P.Context, P.FontDescription) ->
  (Double, Double) ->
  [Text] ->
  R.Render ()
drawTextMultiline (ctxt, desc) (x, y) msgs = do
  for_ (zip [y, y + 12 .. ] msgs) $ \(y', msg) ->
    drawTextLine (ctxt, desc) (x, y') msg

myText :: (P.Context, P.FontDescription, P.FontDescription) -> R.Render ()
myText (pangoCtxt, descSans, descMono) = do
  drawTextMultiline (pangoCtxt, descSans) (120, 120) (T.lines loremIpsum)
  drawTextMultiline (pangoCtxt, descMono) (120, 240) (T.lines loremIpsum)

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
