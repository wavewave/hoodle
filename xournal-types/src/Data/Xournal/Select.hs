{-# LANGUAGE TypeSynonymInstances, TypeOperators, FlexibleInstances, 
             StandaloneDeriving, DeriveFunctor, DeriveFoldable, 
             DeriveTraversable  #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Xournal.Select 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- representing selection of xournal type 
-- 
-----------------------------------------------------------------------------

module Data.Xournal.Select where

import Control.Applicative hiding (empty)
import Control.Compose
import Data.Foldable
import Data.Monoid
import Data.Sequence
import Data.Traversable
-- from this package
import Data.Xournal.Generic
-- 
import Prelude hiding (zipWith, length, splitAt)

-- |
newtype SeqZipper a = SZ { unSZ :: (a, (Seq a,Seq a)) } 

-- |
deriving instance Functor SeqZipper

-- |
deriving instance Foldable SeqZipper 

-- |
instance Applicative SeqZipper where
  pure = singletonSZ 
  SZ (f,(f1s,f2s)) <*> SZ (x,(y1s,y2s)) = SZ (f x, (zipWith id f1s y1s, zipWith id f2s y2s))

-- |
deriving instance Traversable SeqZipper 

-- |
singletonSZ :: a -> SeqZipper a  
singletonSZ x = SZ (x, (empty,empty))

-- |
lengthSZ :: SeqZipper a -> Int 
lengthSZ (SZ (_x, (x1s,x2s))) = length x1s + length x2s + 1 

-- |
currIndex :: SeqZipper a -> Int
currIndex (SZ (_x, (x1s,_x2s))) = length x1s 

-- |
appendGoLast :: SeqZipper a -> a -> SeqZipper a
appendGoLast (SZ (y,(y1s,y2s))) x = SZ (x, ((y1s |> y) >< y2s, empty))

-- |
chopFirst :: SeqZipper a -> Maybe (SeqZipper a)
chopFirst (SZ (y,(y1s,y2s))) = 
  case viewl y1s of
    EmptyL -> case viewl y2s of 
                EmptyL -> Nothing 
                z :< zs -> Just (SZ (z,(empty,zs)))
    _z :< zs -> Just (SZ (y,(zs,y2s)))
    
-- | 
moveLeft :: SeqZipper a -> Maybe (SeqZipper a)
moveLeft (SZ (x,(x1s,x2s))) = 
  case viewr x1s of
    EmptyR -> Nothing 
    zs :> z -> Just (SZ (z,(zs,x<|x2s)))

-- |
moveRight :: SeqZipper a -> Maybe (SeqZipper a) 
moveRight (SZ (x,(x1s,x2s))) = 
  case viewl x2s of 
    EmptyL -> Nothing
    z :< zs -> Just (SZ (z,(x1s|>x,zs)))

-- |
moveTo :: Int -> SeqZipper a -> Maybe (SeqZipper a) 
moveTo n orig@(SZ (x,(x1s,x2s))) = 
  let n_x1s = length x1s 
      n_x2s = length x2s 
      res | n < 0 || n > n_x1s + n_x2s = Nothing 
          | n == n_x1s = Just orig 
          | n < n_x1s = let (x1s1, x1s2) = splitAt n x1s 
                            el :< rm = viewl x1s2
                        in Just (SZ (el, (x1s1,(rm |> x) >< x2s)))
          | n > n_x1s = let (x2s1,x2s2) = splitAt (n-n_x1s-1) x2s
                            el :< rm = viewl x2s2
                        in Just (SZ (el, ((x1s |> x) >< x2s1, rm)))
          | otherwise = error "error in moveTo"
  in res 

-- | 
goFirst :: SeqZipper a -> SeqZipper a 
goFirst orig@(SZ (x,(x1s,x2s))) =
  case viewl x1s of 
    EmptyL -> orig
    z :< zs -> SZ (z,(empty, zs `mappend` (x <| x2s)))  

-- |
goLast :: SeqZipper a -> SeqZipper a 
goLast orig@(SZ (x,(x1s,x2s))) = 
  case viewr x2s of 
    EmptyR -> orig
    zs :> z -> SZ (z,((x1s |> x) `mappend` zs , empty))
 
-- | 
current :: SeqZipper a -> a 
current (SZ (x,(_,_))) = x

-- | 
prev :: SeqZipper a -> Maybe a 
prev = fmap current . moveLeft

-- |
next :: SeqZipper a -> Maybe a 
next = fmap current . moveRight

-- |
replace :: a -> SeqZipper a -> SeqZipper a 
replace y (SZ (_x,zs)) = SZ (y,zs)

-- |
deleteCurrent :: SeqZipper a -> Maybe (SeqZipper a)
deleteCurrent (SZ (_,(xs,ys))) = 
  case viewl ys of 
    EmptyL -> case viewr xs of 
                EmptyR -> Nothing 
                zs :> z -> Just (SZ (z,(zs,ys)))
    z :< zs -> Just (SZ (z,(xs,zs)))

-- |
data ZipperSelect a = NoSelect { allelems :: [a] }  
                    | Select { zipper :: (Maybe :. SeqZipper) a }

-- | 
deriving instance Functor ZipperSelect 

-- |
selectFirst :: ZipperSelect a -> ZipperSelect a 
selectFirst (NoSelect []) = NoSelect []
selectFirst (NoSelect lst@(_:_))  = Select . gFromList $ lst
selectFirst (Select (O Nothing)) = NoSelect []
selectFirst (Select (O msz)) = Select . O $ return . goFirst =<<  msz

-- |
instance GListable (Maybe :. SeqZipper) where
  gFromList [] = O Nothing 
  gFromList (x:xs) = O (Just (SZ (x, (empty,fromList xs))))
  gToList (O Nothing) = [] 
  gToList (O (Just (SZ (x,(xs,ys))))) = toList xs ++ (x : toList ys)

-- |
instance GListable ZipperSelect where
  gFromList xs = NoSelect xs
  gToList (NoSelect xs) = xs 
  gToList (Select xs) = gToList xs

-- | 
deriving instance Foldable ZipperSelect

-- |
deriving instance Traversable ZipperSelect


