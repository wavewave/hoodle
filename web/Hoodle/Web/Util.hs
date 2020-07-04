{-# OPTIONS_GHC -Werror -Wall #-}

module Hoodle.Web.Util
  ( pathBBox,
    findHitStrokes,
  )
where

import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified ForeignJS as J
import GHCJS.Marshal (FromJSVal (..), ToJSVal (..))
import GHCJS.Types (JSVal)
import Hoodle.HitTest (do2BBoxIntersect, doesLineHitStrk)
import Hoodle.HitTest.Type (BBox (..), BBoxed (..), GetBBoxable (getBBox))
import Message (CommitId)
import State (RStroke (..))

-- | find bbox surrounding a path
pathBBox :: [(Double, Double)] -> BBox
pathBBox path =
  let xs = map fst path
      ys = map snd path
   in BBox
        { bbox_upperleft = (minimum xs, minimum ys),
          bbox_lowerright = (maximum xs, maximum ys)
        }

findHitStrokes ::
  JSVal ->
  Seq (Double, Double) ->
  [BBoxed RStroke] ->
  IO [CommitId]
findHitStrokes svg cxys strks = do
  xys_arr <-
    J.js_to_svg_point_array svg =<< toJSValListOf (toList cxys)
  xys <- fromJSValUncheckedListOf xys_arr
  let bbox1 = pathBBox xys
  let pairs = zip xys (tail xys)
      hitstrks = flip concatMap pairs $ \((x0, y0), (x, y)) ->
        map rstrokeCommitId
          $ filter (doesLineHitStrk ((x0, y0), (x, y)) . rstrokePath)
          $ map bbxed_content
          $ filter (do2BBoxIntersect bbox1 . getBBox)
          $ strks
  pure hitstrks
