{-# LANGUAGE JavaScriptFFI #-}

module ForeignJS where

import qualified Data.JSString as JSS
import GHCJS.Foreign.Callback (Callback)
import GHCJS.Marshal (FromJSVal (fromJSValUncheckedListOf), ToJSVal (toJSValListOf))
import GHCJS.Types (JSString, JSVal)

foreign import javascript unsafe "console.log($1)"
  js_console_log :: JSVal -> IO ()

foreign import javascript unsafe "preventDefaultTouchMove()"
  js_prevent_default_touch_move :: IO ()

foreign import javascript unsafe "$r = SVG('#box')"
  js_svg_box :: IO JSVal

foreign import javascript unsafe "$1.on($2,$3)"
  js_on :: JSVal -> JSString -> Callback a -> IO ()

foreign import javascript unsafe "$r = $1.clientX"
  js_clientX :: JSVal -> IO Double

foreign import javascript unsafe "$r = $1.clientY"
  js_clientY :: JSVal -> IO Double

foreign import javascript unsafe "$r = $1.clientX"
  js_x :: JSVal -> IO Double

foreign import javascript unsafe "$r = $1.clientY"
  js_y :: JSVal -> IO Double

foreign import javascript unsafe "$r = toSVGPoint($1,$2,$3)"
  js_to_svg_point :: JSVal -> Double -> Double -> IO JSVal

foreign import javascript unsafe "$r = toSVGPointArray($1,$2)"
  js_to_svg_point_array :: JSVal -> JSVal -> IO JSVal

foreign import javascript unsafe "drawPath($1,$2,$3)"
  js_draw_path :: JSVal -> JSString -> JSVal -> IO ()

foreign import javascript unsafe "window.requestAnimationFrame($1)"
  js_requestAnimationFrame :: Callback a -> IO ()

foreign import javascript unsafe "refresh($1,$2)"
  js_refresh :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$1.addEventListener($2,$3)"
  js_addEventListener :: JSVal -> JSString -> Callback a -> IO ()

foreign import javascript unsafe "overlay_point($1,$2,$3,$4,$5,$6)"
  js_overlay_point :: JSVal -> JSVal -> Double -> Double -> Double -> Double -> IO ()

foreign import javascript unsafe "clear_overlay($1)"
  js_clear_overlay :: JSVal -> IO ()

foreign import javascript unsafe "fix_dpi($1)"
  js_fix_dpi :: JSVal -> IO ()

foreign import javascript unsafe "$r = $1.width"
  js_get_width :: JSVal -> IO Double

foreign import javascript unsafe "$1.width = $2"
  js_set_width :: JSVal -> Double -> IO ()

foreign import javascript unsafe "$r = $1.height"
  js_get_height :: JSVal -> IO Double

foreign import javascript unsafe "$1.height = $2"
  js_set_height :: JSVal -> Double -> IO ()

foreign import javascript unsafe "$r = document.createElement('canvas')"
  js_create_canvas :: IO JSVal

foreign import javascript unsafe "$r = $1.pointerType"
  js_pointer_type :: JSVal -> IO JSString

foreign import javascript unsafe "debug_show($1)"
  js_debug_show :: JSVal -> IO ()

foreign import javascript unsafe "document.getElementById($1)"
  js_document_getElementById :: JSString -> IO JSVal

foreign import javascript unsafe "stroke_change_color($1,$2)"
  js_stroke_change_color :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "stroke_remove($1,$2)"
  js_stroke_remove :: JSVal -> JSString -> IO ()

data PointerType = Mouse | Touch | Pen
  deriving (Show, Eq)

getXY :: JSVal -> IO (Double, Double)
getXY ev = (,) <$> js_clientX ev <*> js_clientY ev

getXYinSVG :: JSVal -> (Double, Double) -> IO (Double, Double)
getXYinSVG svg (x0, y0) = do
  r <- js_to_svg_point svg x0 y0
  [x, y] <- fromJSValUncheckedListOf r
  pure (x, y)

getPointerType :: JSVal -> IO PointerType
getPointerType ev = js_pointer_type ev >>= \s -> do
  case JSS.unpack s of
    "touch" -> pure Touch
    "pen" -> pure Pen
    _ -> pure Mouse

drawPath :: JSVal -> String -> [(Double, Double)] -> IO ()
drawPath svg id' xys = do
  arr <- toJSValListOf xys
  js_draw_path svg (JSS.pack id') arr

strokeChangeColor :: JSVal -> String -> IO ()
strokeChangeColor svg id' =
  js_stroke_change_color svg (JSS.pack id')

strokeRemove :: JSVal -> String -> IO ()
strokeRemove svg id' =
  js_stroke_remove svg (JSS.pack id')
