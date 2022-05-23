{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Hoodle.HitTest.Type
  ( BBox (..),
    BBoxed (..),
    GetBBoxable (..),
    AlterList (..),
    NotHitted (..),
    Hitted (..),
    TAlterHitted,
    TEitherAlterHitted (..),
    getA,
    getB,
    interleave,
    takeHitted,
    isAnyHitted,
  )
where

import Control.Monad (liftM2)
import Data.Bifunctor (Bifunctor (..))
import Data.Serialize (Serialize (..))
import Prelude hiding (fst, snd)

-- | bounding box type
data BBox = BBox
  { bbox_upperleft :: (Double, Double),
    bbox_lowerright :: (Double, Double)
  }
  deriving (Show, Eq, Ord)

data BBoxed a = BBoxed
  { bbxed_content :: a,
    bbxed_bbx :: BBox
  }

deriving instance (Show a) => Show (BBoxed a)

deriving instance (Eq a) => Eq (BBoxed a)

deriving instance (Ord a) => Ord (BBoxed a)

-- | orphan instance for BBox
instance Serialize BBox where
  put BBox {..} = put bbox_upperleft >> put bbox_lowerright
  get = liftM2 BBox get get

-- |
class GetBBoxable a where
  getBBox :: a -> BBox

instance GetBBoxable (BBoxed a) where
  getBBox = bbxed_bbx

-- |
data AlterList a b = Empty | a :- AlterList b a
  deriving (Show)

infixr 6 :-

instance Bifunctor AlterList where
  bimap _ _ Empty = Empty
  bimap f g (x :- ys) = f x :- bimap g f ys

-- |
getA :: AlterList a b -> [a]
getA Empty = []
getA (x :- xs) = x : getB xs

-- |
getB :: AlterList a b -> [b]
getB Empty = []
getB (_x :- xs) = getA xs

-- |
newtype NotHitted a = NotHitted {unNotHitted :: [a]}
  deriving (Show, Functor)

-- |
newtype Hitted a = Hitted {unHitted :: [a]}
  deriving (Show, Functor)

-- |
interleave :: (a -> c) -> (b -> c) -> AlterList a b -> [c]
interleave _fa _fb Empty = []
interleave fa fb (x :- xs) = fa x : interleave fb fa xs

----

-- |
type TAlterHitted a = AlterList [a] (Hitted a)

-- |
newtype TEitherAlterHitted a = TEitherAlterHitted
  { unTEitherAlterHitted :: Either [a] (TAlterHitted a)
  }

-- |
takeHitted :: AlterList x (Hitted a) -> [a]
takeHitted = concatMap unHitted . getB

-- |
isAnyHitted :: AlterList x (Hitted a) -> Bool
isAnyHitted = not . null . takeHitted
