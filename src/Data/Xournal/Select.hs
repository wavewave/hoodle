{-# LANGUAGE TypeSynonymInstances, TypeOperators, FlexibleInstances, 
             StandaloneDeriving, DeriveFunctor, DeriveFoldable, DeriveTraversable  #-}

module Data.Xournal.Select where

import Control.Applicative hiding (empty)
import Control.Compose
import Data.Foldable
import Data.Traversable
import Data.Sequence
import Data.Xournal.Generic

import Prelude hiding (zipWith)

newtype SeqZipper a = SZ { unSZ :: (a, (Seq a,Seq a)) } 

deriving instance Functor SeqZipper
deriving instance Foldable SeqZipper 

instance Applicative SeqZipper where
  pure = singletonSZ 
  SZ (f,(f1s,f2s)) <*> SZ (x,(y1s,y2s)) = SZ (f x, (zipWith id f1s y1s, zipWith id f2s y2s))

deriving instance Traversable SeqZipper 

singletonSZ :: a -> SeqZipper a  
singletonSZ x = SZ (x, (empty,empty))

appendGoLast :: SeqZipper a -> a -> SeqZipper a
appendGoLast (SZ (y,(y1s,y2s))) x = SZ (x, ((y1s |> y) >< y2s, empty))

chopFirst :: SeqZipper a -> Maybe (SeqZipper a)
chopFirst (SZ (y,(y1s,y2s))) = 
  case viewl y1s of
    EmptyL -> case viewl y2s of 
                EmptyL -> Nothing 
                z :< zs -> Just (SZ (z,(empty,zs)))
    z :< zs -> Just (SZ (y,(zs,y2s)))
    
moveLeft :: SeqZipper a -> Maybe (SeqZipper a)
moveLeft (SZ (x,(x1s,x2s))) = 
  case viewr x1s of
    EmptyR -> Nothing 
    zs :> z -> Just (SZ (z,(zs,x<|x2s)))

moveRight :: SeqZipper a -> Maybe (SeqZipper a) 
moveRight (SZ (x,(x1s,x2s))) = 
  case viewl x2s of 
    EmptyL -> Nothing
    z :< zs -> Just (SZ (z,(x1s|>x,zs)))


current :: SeqZipper a -> a 
current (SZ (x,(_,_))) = x

prev :: SeqZipper a -> Maybe a 
prev = fmap current . moveLeft

next :: SeqZipper a -> Maybe a 
next = fmap current . moveRight

-- type MSeqZipper = Maybe :. SeqZipper

-- MSZ { unMSZ :: Maybe (SeqZipper a) }

data ZipperSelect a = NoSelect { allelems :: [a] }  
                    | Select { zipper :: (Maybe :. SeqZipper) a }

-- instance Functor SeqZipper where
--  fmap f (SZ (a, (xs,ys))) = SZ (f a, (fmap f xs, fmap f ys))

{-
instance Functor MSeqZipper where
  fmap f (MSZ x) = MSZ (fmap (fmap f) x)
-}

instance Functor ZipperSelect where 
  fmap f (NoSelect as) = NoSelect (fmap f as) 
  fmap f (Select as) = Select (fmap f as)

instance GListable (Maybe :. SeqZipper) where
  gFromList [] = O Nothing 
  gFromList (x:xs) = O (Just (SZ (x, (fromList xs,empty))))
  gToList (O Nothing) = [] 
  gToList (O (Just (SZ (x,(xs,ys))))) = toList xs ++ (x : toList ys)

instance GListable ZipperSelect where
  gFromList xs = NoSelect xs
  gToList (NoSelect xs) = xs 
  gToList (Select xs) = gToList xs


deriving instance Foldable ZipperSelect

deriving instance Traversable ZipperSelect

{-
instance Foldable ZipperSelect where

  
instance Traversable ZipperSelect where
-}