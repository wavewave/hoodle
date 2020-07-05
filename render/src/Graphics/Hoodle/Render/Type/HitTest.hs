-- deprecated. will be removed
module Graphics.Hoodle.Render.Type.HitTest
  ( AlterList (..), -- re-export
    NotHitted (..), -- re-export
    Hitted (..), -- re-export
    TAlterHitted, -- re-export
    TEitherAlterHitted (..), -- re-export
    -- fmapAL, -- re-export
    getA, -- re-export
    getB, -- re-export
    interleave, -- re-export
    takeHitted, -- re-export
    isAnyHitted, -- re-export
    StrokeHitted,
    RItemHitted,
    takeFirstFromHitted,
    takeLastFromHitted,
  )
where

import Control.Applicative
import Data.Hoodle.BBox
import Data.Hoodle.Simple
import Graphics.Hoodle.Render.Type.Item
import Hoodle.HitTest.Type
  ( AlterList (..),
    Hitted (..),
    NotHitted (..),
    TAlterHitted,
    TEitherAlterHitted (..),
    -- fmapAL,
    getA,
    getB,
    interleave,
    isAnyHitted,
    takeHitted,
  )
import Prelude hiding (fst, snd)

-- |
type StrokeHitted = AlterList (NotHitted (BBoxed Stroke)) (Hitted (BBoxed Stroke))

-- |
type RItemHitted = AlterList (NotHitted RItem) (Hitted RItem)

-- |
takeFirstFromHitted :: RItemHitted -> RItemHitted
takeFirstFromHitted Empty = Empty
takeFirstFromHitted (a :- Empty) = (a :- Empty)
takeFirstFromHitted (a :- b :- xs) =
  let (b1, bs) = splitAt 1 (unHitted b)
      rs = concat $ interleave unNotHitted unHitted xs
   in a :- Hitted b1 :- NotHitted (bs ++ rs) :- Empty

-- |
takeLastFromHitted :: RItemHitted -> RItemHitted
takeLastFromHitted Empty = Empty
takeLastFromHitted (a :- Empty) = (a :- Empty)
takeLastFromHitted (a :- b :- Empty) =
  let b' = unHitted b
   in if (not . null) b'
        then
          let (bs, b1) = (,) <$> init <*> last $ b'
           in NotHitted (unNotHitted a ++ bs) :- Hitted [b1] :- Empty
        else NotHitted (unNotHitted a ++ b') :- Empty
takeLastFromHitted (a1 :- b :- a2 :- Empty) =
  let b' = unHitted b
   in if (not . null) b'
        then
          let (bs, b1) = (,) <$> init <*> last $ b'
           in NotHitted (unNotHitted a1 ++ bs) :- Hitted [b1] :- a2 :- Empty
        else NotHitted (unNotHitted a1 ++ b' ++ unNotHitted a2) :- Empty
takeLastFromHitted (a :- b :- xs) =
  let xs' = takeLastFromHitted xs
   in case xs' of
        Empty ->
          let b' = unHitted b
           in if (not . null) b'
                then
                  let (bs, b1) = (,) <$> init <*> last $ b'
                   in NotHitted (unNotHitted a ++ bs) :- Hitted [b1] :- Empty
                else NotHitted (unNotHitted a ++ b') :- Empty
        a' :- Empty ->
          let b' = unHitted b
           in if (not . null) b'
                then
                  let (bs, b1) = (,) <$> init <*> last $ b'
                   in NotHitted (unNotHitted a ++ bs) :- Hitted [b1] :- a' :- Empty
                else NotHitted (unNotHitted a ++ b' ++ unNotHitted a') :- Empty
        a' :- b' :- xs'' -> NotHitted (unNotHitted a ++ unHitted b ++ unNotHitted a') :- b' :- xs''
