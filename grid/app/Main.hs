{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Foldable (for_)
import Data.GI.Base (AttrOp ((:=)), new, on)
import Data.GI.Gtk.Threading (postGUIASync)
import Data.IORef (newIORef, modifyIORef', readIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Traversable (for)
import GI.Cairo.Render qualified as R
import GI.Cairo.Render.Connector as RC
import GI.Gdk qualified as Gdk
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

data ViewPort = ViewPort (Double, Double) (Double, Double)
  deriving (Show)

data GridState = GridState
  { gridViewPort :: ViewPort
  , gridTempViewPort :: Maybe ViewPort
  }
  deriving (Show)

-- | zoom
transformZoom ::
  (Double, Double) ->
  Double ->
  ViewPort ->
  ViewPort
transformZoom (rx, ry) scale vp = vp'
  where
    ViewPort (x0, y0) (x1, y1) = vp
    x = x0 + (x1 - x0) * rx
    y = y0 + (y1 - y0) * ry
    x0' = x + (x0 - x) / scale
    y0' = y + (y0 - y) / scale
    x1' = x + (x1 - x) / scale
    y1' = y + (y1 - y) / scale
    vp' = ViewPort (x0', y0') (x1', y1')

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

myDraw ::
  (P.Context, P.FontDescription, P.FontDescription) ->
  GridState ->
  R.Render ()
myDraw (pangoCtxt, descSans, descMono) s = do
  let (cx0, cy0) = (0, 0)
      (cx1, cy1) = (640, 480)
      ViewPort (vx0, vy0) (vx1, vy1) = fromMaybe (gridViewPort s) (gridTempViewPort s)
      scaleX = (cx1 - cx0) / (vx1 - vx0)
      scaleY = (cy1 - cy0) / (vy1 - vy0)
  R.save
  R.rectangle cx0 cy0 (cx1 - cx0) (cy1 - cy0)
  R.clip
  --
  R.translate cx0 cy0
  R.scale scaleX scaleY
  R.translate (-vx0) (-vy0)
  --
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
  ref <- newIORef (GridState (ViewPort (0, 0) (640, 480)) Nothing)
  _ <- Gtk.init Nothing
  Just (pangoCtxt, descSans, descMono) <- initFont
  mainWindow <- new Gtk.Window [#type := Gtk.WindowTypeToplevel]
  drawingArea <- new Gtk.DrawingArea []
  #addEvents
    drawingArea
    [ Gdk.EventMaskTouchpadGestureMask
    ]
  _ <- drawingArea `on` #draw $
    RC.renderWithContext $ do
      s <- R.liftIO $ readIORef ref
      R.liftIO $ print s
      myDraw (pangoCtxt, descSans, descMono) s
      pure True
  gzoom <- Gtk.gestureZoomNew drawingArea
  _ <- gzoom
    `on` #scaleChanged
    $ \scale -> do
      (_, x, y) <- #getBoundingBoxCenter gzoom
      modifyIORef' ref $ \s ->
        let vp = gridViewPort s
            (cx0, cy0) = (0, 0)
            (cx1, cy1) = (640, 480)
            rx = (x - cx0) / (cx1 - cx0)
            ry = (y - cy0) / (cy1 - cy0)
            mtvp = Just $ transformZoom (rx, ry) scale vp
         in s
              { gridTempViewPort = mtvp
              }
      postGUIASync (#queueDraw drawingArea)
  _ <- gzoom
    `on` #end
    $ \_ -> do
      modifyIORef' ref $ \s ->
        let vp = gridViewPort s
            mtvp = gridTempViewPort s
         in s
              { gridViewPort = fromMaybe vp mtvp
              , gridTempViewPort = Nothing
              }
      postGUIASync (#queueDraw drawingArea)
  layout <- do
    vbox <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 0]
    #packStart vbox drawingArea True True 0
    pure vbox
  _ <- mainWindow `on` #destroy $ Gtk.mainQuit
  #add mainWindow layout
  #setDefaultSize mainWindow 640 480
  #showAll mainWindow
  Gtk.main
