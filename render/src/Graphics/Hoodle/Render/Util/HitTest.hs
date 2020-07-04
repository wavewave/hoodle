{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- NOTE: this module will be deprecated.

module Graphics.Hoodle.Render.Util.HitTest
  ( isPointInBBox, -- re-export
    do2LinesIntersect, -- re-export
    do2BBoxIntersect, -- re-export
    isBBox2InBBox1, -- re-export
    hltHittedByBBox, -- re-export
    hltEmbeddedByBBox, -- re-export
    hltHittedByLineRough, -- re-export
    elimHitted, -- re-export
    merge, -- re-export
    getTotalBBox, -- re-export
    --
    doesLineHitStrk,
    hltFilteredBy_StateT,
    hltFilteredBy,
    hltItmsHittedByLine_StateT,
    hltItmsHittedByLineFrmSelected_StateT,
  )
where

import Control.Monad.State (State, evalState, modify)
import Data.Hoodle.BBox (bbxed_content)
import Data.Hoodle.Simple
  ( Stroke (Stroke, VWStroke, stroke_data),
  )
import Data.Strict.Tuple (Pair ((:!:)))
import Graphics.Hoodle.Render.Type.HitTest
  ( AlterList ((:-), Empty),
    RItemHitted,
  )
import Graphics.Hoodle.Render.Type.Item
  ( RItem (RItemStroke),
  )
import Hoodle.HitTest
  ( do2BBoxIntersect,
    do2LinesIntersect,
    elimHitted,
    getTotalBBox,
    hltEmbeddedByBBox,
    hltHittedByBBox,
    hltHittedByLineRough,
    isBBox2InBBox1,
    isPointInBBox,
    merge,
  )
import Hoodle.HitTest.Type
  ( Hitted (..),
    NotHitted (..),
  )

--------------------------
-- hit test collections --
--------------------------

-- | previously, hitTestLineStroke
doesLineHitStrk :: ((Double, Double), (Double, Double)) -> Stroke -> Bool
doesLineHitStrk line1 str@(Stroke _t _c _w _d) = test (stroke_data str)
  where
    test [] = False
    test ((_ :!: _) : []) = False
    test ((x0 :!: y0) : (x :!: y) : rest) =
      do2LinesIntersect line1 ((x0, y0), (x, y))
        || test ((x :!: y) : rest)
doesLineHitStrk line1 (VWStroke _t _c d) = test d
  where
    test [] = False
    test ((_, _, _) : []) = False
    test ((x0, y0, _) : (x, y, z) : rest) =
      do2LinesIntersect line1 ((x0, y0), (x, y))
        || test ((x, y, z) : rest)

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
hltItmsHittedByLine_StateT ::
  ((Double, Double), (Double, Double)) ->
  [RItem] ->
  State Bool RItemHitted
hltItmsHittedByLine_StateT line = hltFilteredBy_StateT test
  where
    test (RItemStroke strk) = (doesLineHitStrk line . bbxed_content) strk
    test _ = False

-- |
hltItmsHittedByLineFrmSelected_StateT ::
  ((Double, Double), (Double, Double)) ->
  RItemHitted ->
  State Bool (AlterList (NotHitted RItem) RItemHitted)
hltItmsHittedByLineFrmSelected_StateT _ Empty =
  error "something is wrong, invariant broken"
hltItmsHittedByLineFrmSelected_StateT _ (n :- Empty) = return (n :- Empty)
hltItmsHittedByLineFrmSelected_StateT line (n :- h :- rest) = do
  h' <- hltItmsHittedByLine_StateT line (unHitted h)
  (n :-) . (h' :-) <$> hltItmsHittedByLineFrmSelected_StateT line rest
