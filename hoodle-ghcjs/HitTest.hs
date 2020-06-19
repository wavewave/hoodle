module HitTest where

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

-- | previously, hitTestLineStroke
doesLineHitStrk :: ((Double, Double), (Double, Double)) -> [(Double, Double)] -> Bool
doesLineHitStrk _ [] = False
doesLineHitStrk _ (_ : []) = False
doesLineHitStrk line1 ((x0, y0) : (x, y) : rest) =
  do2LinesIntersect line1 ((x0, y0), (x, y)) || doesLineHitStrk line1 ((x, y) : rest)
