{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Hoodle.BBox
  ( BBox (..), -- re-export
    BBoxed (..), -- re-export
    GetBBoxable (..), -- re-export
    MakeBBoxedable (..),
    mkbbox,
    mkbboxF,
    bboxFromStroke,
    bboxFromImage,
    bboxFromSVG,
    dimToBBox,
    bboxToDim,
    xformBBox,
    inflate,
    moveBBoxToOrigin,
    moveBBoxByOffset,
    moveBBoxULCornerTo,
    intersectBBox,
    unionBBox,
    ULMaybe (..),
    IntersectBBox (..),
    UnionBBox (..),
    Maybeable (..),
    bbox4All,
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import qualified Data.Foldable as F
import Data.Hoodle.Simple
import Data.Hoodle.Util
import Data.Monoid
import Data.Strict.Tuple
import Hoodle.HitTest.Type
  ( BBox (..),
    BBoxed (..),
    GetBBoxable (..),
  )
import Prelude hiding (fst, snd)
import qualified Prelude (fst, snd)

-- |
class (Monad m) => MakeBBoxedable m a where
  makeBBoxed :: a -> m (BBoxed a)

instance MakeBBoxedable Identity Stroke where
  makeBBoxed strk = return (BBoxed strk (bboxFromStroke strk))

instance MakeBBoxedable Identity Image where
  makeBBoxed img = return (BBoxed img (bboxFromImage img))

instance MakeBBoxedable Identity SVG where
  makeBBoxed svg = return (BBoxed svg (bboxFromSVG svg))

instance MakeBBoxedable Identity Link where
  makeBBoxed lnk = return (BBoxed lnk (bboxFromLink lnk))

instance MakeBBoxedable Identity Anchor where
  makeBBoxed anc = return (BBoxed anc (bboxFromAnchor anc))

-- |
mkbbox :: [Pair Double Double] -> BBox
mkbbox lst =
  let xs = map fst lst
      ys = map snd lst
   in BBox
        { bbox_upperleft = (minimum xs, minimum ys),
          bbox_lowerright = (maximum xs, maximum ys)
        }

-- |
mkbboxF :: (F.Foldable m, Functor m) => m (Double, Double) -> BBox
mkbboxF lst =
  let xs = fmap Prelude.fst lst
      ys = fmap Prelude.snd lst
   in BBox
        { bbox_upperleft = (F.minimum xs, F.minimum ys),
          bbox_lowerright = (F.maximum xs, F.maximum ys)
        }

-- |
bboxFromStroke :: Stroke -> BBox
bboxFromStroke (Stroke _ _ w dat) = inflate (mkbbox dat) w
bboxFromStroke (VWStroke _ _ dat) =
  let dat' = map ((,) <$> fst3 <*> snd3) dat
      widthmax = F.maximum (map trd3 dat)
   in inflate (mkbboxF dat') widthmax

-- |
dimToBBox :: Dimension -> BBox
dimToBBox (Dim w h) = BBox (0, 0) (w, h)

-- |
-- |
bboxToDim :: BBox -> Dimension
bboxToDim (BBox (x1, y1) (x2, y2)) = Dim (x2 - x1) (y2 - y1)

-- |
bboxFromImage :: Image -> BBox
bboxFromImage (Image _ (x, y) d) = moveBBoxULCornerTo (x, y) (dimToBBox d)

-- |
bboxFromSVG :: SVG -> BBox
bboxFromSVG (SVG _ _ _ (x, y) d) = moveBBoxULCornerTo (x, y) (dimToBBox d)

-- |
bboxFromLink :: Link -> BBox
bboxFromLink (Link _ _ _ _ _ _ (x, y) d) = moveBBoxULCornerTo (x, y) (dimToBBox d)
bboxFromLink (LinkDocID _ _ _ _ _ _ (x, y) d) = moveBBoxULCornerTo (x, y) (dimToBBox d)
bboxFromLink (LinkAnchor _ _ _ _ _ (x, y) d) = moveBBoxULCornerTo (x, y) (dimToBBox d)

-- |
bboxFromAnchor :: Anchor -> BBox
bboxFromAnchor (Anchor _ _ (x, y) d) = moveBBoxULCornerTo (x, y) (dimToBBox d)

-- | general transform BBox
xformBBox :: ((Double, Double) -> (Double, Double)) -> BBox -> BBox
xformBBox f (BBox c1 c2) = BBox (f c1) (f c2)

-- | inflate bbox by amount r
inflate :: BBox -> Double -> BBox
inflate (BBox (x1, y1) (x2, y2)) r = BBox (x1 - r, y1 - r) (x2 + r, y2 + r)

-- |
moveBBoxToOrigin :: BBox -> BBox
moveBBoxToOrigin (BBox (x0, y0) (x1, y1)) = BBox (0, 0) (x1 - x0, y1 - y0)

-- |
moveBBoxByOffset :: (Double, Double) -> BBox -> BBox
moveBBoxByOffset (xoff, yoff) (BBox (x0, y0) (x1, y1)) = BBox (x0 + xoff, y0 + yoff) (x1 + xoff, y1 + yoff)

-- |
moveBBoxULCornerTo :: (Double, Double) -> BBox -> BBox
moveBBoxULCornerTo (x, y) b@(BBox (x0, y0) _) = moveBBoxByOffset (x - x0, y - y0) b

-- |
intersectBBox :: BBox -> BBox -> Maybe BBox
intersectBBox (BBox (x1, y1) (x2, y2)) (BBox (x3, y3) (x4, y4)) = do
  guard $ (x1 <= x3 && x3 <= x2) || (x3 <= x1 && x1 <= x4)
  guard $ (y1 <= y3 && y3 <= y2) || (y3 <= y1 && y1 <= y4)
  let x5 = if x1 <= x3 then x3 else x1
      y5 = if y1 <= y3 then y3 else y1
      x6 = min x2 x4
      y6 = min y2 y4
  return (BBox (x5, y5) (x6, y6))

-- |
unionBBox :: BBox -> BBox -> BBox
unionBBox (BBox (x1, y1) (x2, y2)) (BBox (x3, y3) (x4, y4)) =
  let x5 = if x1 < x3 then x1 else x3
      y5 = if y1 < y3 then y1 else y3
      x6 = if x2 < x4 then x4 else x2
      y6 = if y2 < y4 then y4 else y2
   in BBox (x5, y5) (x6, y6)

-- |
data ULMaybe a = Bottom | Middle a | Top

deriving instance Show a => Show (ULMaybe a)

deriving instance Eq a => Eq (ULMaybe a)

-- |
newtype IntersectBBox = Intersect {unIntersect :: ULMaybe BBox}
  deriving (Show, Eq)

-- |
newtype UnionBBox = Union {unUnion :: ULMaybe BBox}
  deriving (Show, Eq)

instance Semigroup IntersectBBox where
  (Intersect Bottom) <> _ = Intersect Bottom
  _ <> (Intersect Bottom) = Intersect Bottom
  (Intersect Top) <> x = x
  x <> (Intersect Top) = x
  (Intersect (Middle x)) <> (Intersect (Middle y)) =
    maybe (Intersect Bottom) (Intersect . Middle) (x `intersectBBox` y)

instance Monoid IntersectBBox where
  mempty = Intersect Top

instance Semigroup UnionBBox where
  (Union Bottom) <> x = x
  x <> (Union Bottom) = x
  (Union Top) <> _ = Union Top
  _ <> (Union Top) = Union Top
  (Union (Middle x)) <> (Union (Middle y)) = Union (Middle (x `unionBBox` y))

instance Monoid UnionBBox where
  mempty = Union Bottom

-- |
class Maybeable a where
  type ElemType a :: *
  toMaybe :: a -> Maybe (ElemType a)
  fromMaybe :: Maybe (ElemType a) -> a

instance Maybeable IntersectBBox where
  type ElemType IntersectBBox = BBox
  toMaybe (Intersect Bottom) = Nothing
  toMaybe (Intersect Top) = Nothing
  toMaybe (Intersect (Middle x)) = Just x
  fromMaybe Nothing = Intersect Top
  fromMaybe (Just x) = Intersect (Middle x)

instance Maybeable UnionBBox where
  type ElemType UnionBBox = BBox
  toMaybe (Union Bottom) = Nothing
  toMaybe (Union Top) = Nothing
  toMaybe (Union (Middle x)) = Just x
  fromMaybe Nothing = Union Top
  fromMaybe (Just x) = Union (Middle x)

-- |
bbox4All :: (F.Foldable t, Functor t, GetBBoxable a) => t a -> ULMaybe BBox
bbox4All = unUnion . F.fold . fmap (Union . Middle . getBBox)
