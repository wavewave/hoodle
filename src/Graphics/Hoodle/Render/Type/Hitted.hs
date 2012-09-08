-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.Type.Hitted 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Selection type
--
-----------------------------------------------------------------------------

module Graphics.Hoodle.Render.Type.Hitted where

import Data.Hoodle.BBox
--
import Prelude hiding (fst,snd)

-- |
data AlterList a b = Empty | a :- AlterList b a
                   deriving (Show)

infixr 6 :-

-- | 
newtype NotHitted a = NotHitted { unNotHitted :: [a] } 
                    deriving (Show)

-- | 
newtype Hitted a = Hitted { unHitted :: [a] } 
                   deriving (Show)

-- | 
type StrokeHitted = AlterList (NotHitted StrokeBBox) (Hitted StrokeBBox) 

-- | 
fmapAL :: (a -> c) -> (b -> d) -> AlterList a b -> AlterList c d
fmapAL _ _ Empty = Empty 
fmapAL f g (x :- ys) = f x :- fmapAL g f ys 

-- | 
getA :: AlterList a b -> [a] 
getA Empty = [] 
getA (x :- xs)  = x : getB xs 

-- | 
getB :: AlterList a b -> [b]
getB Empty = [] 
getB (_x :- xs) = getA xs 

-- | 
interleave :: (a->c) -> (b->c) -> AlterList a b -> [c]
interleave _fa _fb Empty = [] 
interleave fa fb (x :- xs) = fa x : (interleave fb fa xs) 

----

-- | 
type TAlterHitted a = AlterList [a] (Hitted a)

-- | 
newtype TEitherAlterHitted a = 
          TEitherAlterHitted { 
            unTEitherAlterHitted :: Either [a] (TAlterHitted a)
          }


