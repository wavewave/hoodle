module Hoodle.HitTest where

import Control.Monad.State (MonadState (get, put), State, evalState, modify)
import Hoodle.HitTest.Type
  ( AlterList ((:-), Empty),
    BBox (..),
    GetBBoxable (..),
    Hitted (..),
    NotHitted (..),
  )

--------------------------
-- hit test collections --
--------------------------

-- | hit test of whether a point in a bbox
--   previously, hitTestBBoxPoint
isPointInBBox ::
  BBox ->
  -- | point
  (Double, Double) ->
  Bool
isPointInBBox (BBox (ulx, uly) (lrx, lry)) (x, y) =
  ulx <= x && x <= lrx && uly <= y && y <= lry

-- | hit test of whether two lines intersect
do2LinesIntersect ::
  -- | line1
  ((Double, Double), (Double, Double)) ->
  -- | line2
  ((Double, Double), (Double, Double)) ->
  Bool
do2LinesIntersect ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) =
  (x2 - xc) * (xc - x1) > 0 && (x3 - xc) * (xc - x4) > 0
  where
    x21 = x2 - x1
    x43 = x4 - x3
    y21 = y2 - y1
    y43 = y4 - y3
    xc = (x21 * x43 * (y3 - y1) + y21 * x43 * x1 - y43 * x21 * x3) / (y21 * x43 - y43 * x21)

-- | Does a line hit a stroke (a sequence of points)?
doesLineHitStrk :: ((Double, Double), (Double, Double)) -> [(Double, Double)] -> Bool
doesLineHitStrk _ [] = False
doesLineHitStrk _ (_ : []) = False
doesLineHitStrk line1 ((x0, y0) : (x, y) : rest) =
  do2LinesIntersect line1 ((x0, y0), (x, y)) || doesLineHitStrk line1 ((x, y) : rest)

-- | Do two bounding boxes intersect with each other?
--   previously, hitTestBBoxBBox
do2BBoxIntersect :: BBox -> BBox -> Bool
do2BBoxIntersect (BBox (ulx1, uly1) (lrx1, lry1)) (BBox (ulx2, uly2) (lrx2, lry2)) =
  p1 && p2
  where
    p1 =
      ulx1 <= ulx2 && ulx2 <= lrx1
        || ulx1 <= lrx2 && lrx2 <= lrx1
        || ulx2 <= ulx1 && ulx1 <= lrx2
        || ulx2 <= lrx1 && lrx1 <= lrx2
    p2 =
      uly1 <= uly2 && uly2 <= lry1
        || uly1 <= lry2 && lry2 <= lry1
        || uly2 <= uly1 && uly1 <= lry2
        || uly2 <= lry1 && lry1 <= lry2

-- | is the second bbox inside the first bbox?
--   previously, hitTestInsideBBox
isBBox2InBBox1 ::
  -- | 1st bbox
  BBox ->
  -- | 2nd bbox
  BBox ->
  Bool
isBBox2InBBox1 b1 (BBox (ulx2, uly2) (lrx2, lry2)) =
  isPointInBBox b1 (ulx2, uly2) && isPointInBBox b1 (lrx2, lry2)

--------------------------------------------------------
-- item filtering functions that results in AlterList --
--------------------------------------------------------

-- |
hltFilteredBy_StateT ::
  -- | hit test condition
  (a -> Bool) ->
  -- | strokes to test
  [a] ->
  State Bool (AlterList (NotHitted a) (Hitted a))
hltFilteredBy_StateT test itms = do
  let (nhit, rest) = break test itms
      (hit, rest') = break (not . test) rest
  modify (|| (not . null) hit)
  if null rest'
    then return (NotHitted nhit :- Hitted hit :- NotHitted [] :- Empty)
    else return (NotHitted nhit :- Hitted hit :- hltFilteredBy test rest')

-- | highlight strokes filtered by a condition.
--   previously mkHitTestAL
hltFilteredBy ::
  -- | hit test condition
  (a -> Bool) ->
  -- | strokes to test
  [a] ->
  AlterList (NotHitted a) (Hitted a)
hltFilteredBy test is = evalState (hltFilteredBy_StateT test is) False

-- |
hltHittedByBBox ::
  (GetBBoxable a) =>
  -- | test bounding box
  BBox ->
  -- | items to test
  [a] ->
  AlterList (NotHitted a) (Hitted a)
hltHittedByBBox b = hltFilteredBy (do2BBoxIntersect b . getBBox)

-- |
hltEmbeddedByBBox ::
  (GetBBoxable a) =>
  BBox ->
  [a] ->
  AlterList (NotHitted a) (Hitted a)
hltEmbeddedByBBox b = hltFilteredBy (isBBox2InBBox1 b . getBBox)

-- | only check if a line and bbox of item overlapped
hltHittedByLineRough ::
  (GetBBoxable a) =>
  -- | line
  ((Double, Double), (Double, Double)) ->
  -- | items to test
  [a] ->
  AlterList (NotHitted a) (Hitted a)
hltHittedByLineRough (p1, p2) = hltFilteredBy boxhittest
  where
    boxhittest s =
      isPointInBBox (getBBox s) p1
        || isPointInBBox (getBBox s) p2

-- |
elimHitted ::
  (GetBBoxable a) =>
  AlterList (NotHitted a) (Hitted a) ->
  State (Maybe BBox) [a]
elimHitted Empty = error "something wrong in elimHitted"
elimHitted (n :- Empty) = return (unNotHitted n)
elimHitted (n :- h :- rest) = do
  bbox <- get
  let bbox2 = getTotalBBox (unHitted h)
  put (merge bbox bbox2)
  return . (unNotHitted n ++) =<< elimHitted rest

-- |
merge :: Maybe BBox -> Maybe BBox -> Maybe BBox
merge Nothing Nothing = Nothing
merge Nothing (Just b) = Just b
merge (Just b) Nothing = Just b
merge (Just (BBox (x1, y1) (x2, y2))) (Just (BBox (x3, y3) (x4, y4))) =
  Just (BBox (min x1 x3, min y1 y3) (max x2 x4, max y2 y4))

-- |
getTotalBBox :: (GetBBoxable a) => [a] -> Maybe BBox
getTotalBBox = foldl f Nothing
  where
    f acc = merge acc . Just . getBBox
