{-# LANGUAGE OverloadedStrings #-}

module Hoodle.Web.Util
  ( arrayBufferToByteString,
    bytestringToArrayBuffer,
    pathBBox,
    intersectingStrokes,
    enclosedStrokes,
    putStrLnAndFlush,
    sendBinary,
    stringifyStrokeId,
    transformPathFromCanvasToSVG,
  )
where

import Data.Binary (Binary, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import Data.Semigroup (Semigroup ((<>)))
import Data.Sequence (Seq)
import Data.String (IsString (..))
import qualified GHCJS.Buffer as Buffer
import GHCJS.Marshal (FromJSVal (..), ToJSVal (..))
import GHCJS.Types (JSVal)
import Hoodle.HitTest (do2BBoxIntersect, doesLineHitStrk, hitLassoPoint)
import Hoodle.HitTest.Type (BBox (..), BBoxed (..), GetBBoxable (getBBox))
import qualified Hoodle.Web.ForeignJS as J
import Hoodle.Web.Type.State (RStroke, rstrokeCommitId, rstrokePath)
import JavaScript.TypedArray.ArrayBuffer (ArrayBuffer)
import qualified JavaScript.Web.WebSocket as WS
import Lens.Micro ((^.))
import Message (CommitId (..))
import System.IO (hFlush, hPutStrLn, stdout)

foreign import javascript unsafe "$3.slice($1, $1 + $2)"
  js_bufferSlice :: Int -> Int -> ArrayBuffer -> ArrayBuffer

bytestringToArrayBuffer :: ByteString -> ArrayBuffer
bytestringToArrayBuffer bs = js_bufferSlice offset len $ Buffer.getArrayBuffer buffer
  where
    (buffer, offset, len) = Buffer.fromByteString bs

arrayBufferToByteString :: ArrayBuffer -> ByteString
arrayBufferToByteString =
  Buffer.toByteString 0 Nothing . Buffer.createFromArrayBuffer

sendBinary :: (Binary a) => WS.WebSocket -> a -> IO ()
sendBinary sock msg =
  let bs = BL.toStrict $ encode msg
      arrbuf = bytestringToArrayBuffer bs
   in WS.sendArrayBuffer arrbuf sock

putStrLnAndFlush :: String -> IO ()
putStrLnAndFlush s = do
  hPutStrLn stdout s
  hFlush stdout

transformPathFromCanvasToSVG :: JSVal -> [(Double, Double)] -> IO [(Double, Double)]
transformPathFromCanvasToSVG svg cxys = do
  path_arr <-
    J.js_to_svg_point_array svg =<< toJSValListOf (toList cxys)
  fromJSValUncheckedListOf path_arr

-- | find bbox surrounding a path
pathBBox :: [(Double, Double)] -> BBox
pathBBox path =
  let xs = map fst path
      ys = map snd path
   in BBox
        { bbox_upperleft = (minimum xs, minimum ys),
          bbox_lowerright = (maximum xs, maximum ys)
        }

intersectingStrokes ::
  [(Double, Double)] ->
  [BBoxed RStroke] ->
  [CommitId]
intersectingStrokes xys strks =
  let bbox1 = pathBBox xys
      pairs = zip xys (tail xys)
      hitstrks = flip concatMap pairs $ \((x0, y0), (x, y)) ->
        map (^. rstrokeCommitId) $
          filter (doesLineHitStrk ((x0, y0), (x, y)) . (^. rstrokePath)) $
            map bbxed_content $
              filter (do2BBoxIntersect bbox1 . getBBox) $
                strks
   in hitstrks

pathEnclosedByLasso :: [(Double, Double)] -> Seq (Double, Double) -> Bool
pathEnclosedByLasso path lasso =
  all (hitLassoPoint lasso) path

enclosedStrokes ::
  Seq (Double, Double) ->
  [BBoxed RStroke] ->
  [CommitId]
enclosedStrokes lasso strks =
  let bbox1 = pathBBox (toList lasso)
   in map (^. rstrokeCommitId) $
        filter ((`pathEnclosedByLasso` lasso) . (^. rstrokePath)) $
          map bbxed_content $
            filter (do2BBoxIntersect bbox1 . getBBox) $
              strks

stringifyStrokeId :: (IsString s, Semigroup s) => CommitId -> s
stringifyStrokeId (CommitId i) = "stroke" <> fromString (show i)
