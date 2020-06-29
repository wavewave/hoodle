{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}

module Hoodle.HitTest.Type where

import Prelude hiding (fst, snd)

-- | bounding box type
data BBox
  = BBox
      { bbox_upperleft :: (Double, Double),
        bbox_lowerright :: (Double, Double)
      }
  deriving (Show, Eq, Ord)

data BBoxed a
  = BBoxed
      { bbxed_content :: a,
        bbxed_bbx :: BBox
      }

deriving instance (Show a) => Show (BBoxed a)

deriving instance (Eq a) => Eq (BBoxed a)

deriving instance (Ord a) => Ord (BBoxed a)

-- |
class GetBBoxable a where
  getBBox :: a -> BBox

instance GetBBoxable (BBoxed a) where
  getBBox = bbxed_bbx

-- |
data AlterList a b = Empty | a :- AlterList b a
  deriving (Show)

infixr 6 :-

-- |
newtype NotHitted a = NotHitted {unNotHitted :: [a]}
  deriving (Show, Functor)

-- |
newtype Hitted a = Hitted {unHitted :: [a]}
  deriving (Show, Functor)

-- |
fmapAL :: (a -> c) -> (b -> d) -> AlterList a b -> AlterList c d
fmapAL _ _ Empty = Empty
fmapAL f g (x :- ys) = f x :- fmapAL g f ys

-- |
getA :: AlterList a b -> [a]
getA Empty = []
getA (x :- xs) = x : getB xs

-- |
getB :: AlterList a b -> [b]
getB Empty = []
getB (_x :- xs) = getA xs

-- |
interleave :: (a -> c) -> (b -> c) -> AlterList a b -> [c]
interleave _fa _fb Empty = []
interleave fa fb (x :- xs) = fa x : (interleave fb fa xs)

----

-- |
type TAlterHitted a = AlterList [a] (Hitted a)

-- |
newtype TEitherAlterHitted a
  = TEitherAlterHitted
      { unTEitherAlterHitted :: Either [a] (TAlterHitted a)
      }

-- |
takeHitted :: AlterList x (Hitted a) -> [a]
takeHitted = concatMap unHitted . getB

-- |
isAnyHitted :: AlterList x (Hitted a) -> Bool
isAnyHitted = not . null . takeHitted
