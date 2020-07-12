{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Werror -Wall #-}

module Hoodle.Web.Util
  ( pathBBox,
    intersectingStrokes,
    enclosedStrokes,
    putStrLnAndFlush,
    stringifyStrokeId,
    transformPathFromCanvasToSVG,
  )
where

import Data.Foldable (toList)
import Data.Semigroup (Semigroup ((<>)))
import Data.Sequence (Seq)
import Data.String (IsString (..))
import GHCJS.Marshal (FromJSVal (..), ToJSVal (..))
import GHCJS.Types (JSVal)
import Hoodle.HitTest (do2BBoxIntersect, doesLineHitStrk, hitLassoPoint)
import Hoodle.HitTest.Type (BBox (..), BBoxed (..), GetBBoxable (getBBox))
import qualified Hoodle.Web.ForeignJS as J
import Hoodle.Web.Type.State (RStroke (..))
import Message (CommitId (..))
import System.IO (hFlush, hPutStrLn, stdout)

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
        map rstrokeCommitId
          $ filter (doesLineHitStrk ((x0, y0), (x, y)) . rstrokePath)
          $ map bbxed_content
          $ filter (do2BBoxIntersect bbox1 . getBBox)
          $ strks
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
   in map rstrokeCommitId
        $ filter ((`pathEnclosedByLasso` lasso) . rstrokePath)
        $ map bbxed_content
        $ filter (do2BBoxIntersect bbox1 . getBBox)
        $ strks

stringifyStrokeId :: (IsString s, Semigroup s) => CommitId -> s
stringifyStrokeId (CommitId i) = "stroke" <> fromString (show i)
