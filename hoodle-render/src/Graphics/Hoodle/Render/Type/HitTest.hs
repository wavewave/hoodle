{-# LANGUAGE DeriveFunctor #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.Type.HitTest 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Selection type
--
-----------------------------------------------------------------------------

module Graphics.Hoodle.Render.Type.HitTest where

import Control.Applicative
-- 
import Data.Hoodle.BBox
import Data.Hoodle.Simple
-- 
import Graphics.Hoodle.Render.Type.Item 
--
import Prelude hiding (fst,snd)

-- |
data AlterList a b = Empty | a :- AlterList b a
                   deriving (Show)

infixr 6 :-

-- | 
newtype NotHitted a = NotHitted { unNotHitted :: [a] } 
                    deriving (Show,Functor)

-- | 
newtype Hitted a = Hitted { unHitted :: [a] } 
                   deriving (Show,Functor)

-- | 
type StrokeHitted = AlterList (NotHitted (BBoxed Stroke)) (Hitted (BBoxed Stroke))

-- | 
type RItemHitted = AlterList (NotHitted RItem) (Hitted RItem) 

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


-- |
takeHitted :: AlterList x (Hitted a) -> [a] 
takeHitted = concatMap unHitted . getB 

-- |
isAnyHitted :: AlterList x (Hitted a) -> Bool 
isAnyHitted = not . null . takeHitted


-- | 
takeFirstFromHitted :: RItemHitted -> RItemHitted 
takeFirstFromHitted Empty = Empty 
takeFirstFromHitted (a :- Empty ) = (a :- Empty )
takeFirstFromHitted (a :- b :- xs ) = 
  let (b1,bs) = splitAt 1 (unHitted b) 
      rs = concat $ interleave unNotHitted unHitted xs   
  in a :- Hitted b1 :- NotHitted (bs ++ rs) :- Empty
     
-- | 
takeLastFromHitted :: RItemHitted -> RItemHitted 
takeLastFromHitted Empty = Empty 
takeLastFromHitted (a :- Empty ) = (a :- Empty )
takeLastFromHitted (a :- b :- Empty ) = 
    let b' = unHitted b
    in if (not.null) b'
       then let (bs,b1) = (,) <$> init <*> last $ b'
            in NotHitted (unNotHitted a ++ bs) :- Hitted [b1] :- Empty
       else NotHitted (unNotHitted a ++ b') :- Empty 
takeLastFromHitted (a1 :- b :- a2 :- Empty ) = 
    let b' = unHitted b  
    in if (not.null) b' 
       then let (bs,b1) = (,) <$> init <*> last $ b'
            in NotHitted (unNotHitted a1 ++ bs) :- Hitted [b1] :- a2 :- Empty 
       else NotHitted (unNotHitted a1 ++ b' ++ unNotHitted a2) :- Empty
takeLastFromHitted (a :- b :- xs ) = 
  let xs' = takeLastFromHitted xs 
  in case xs' of 
       Empty ->       let b' = unHitted b
                      in if (not.null) b'
                         then let (bs,b1) = (,) <$> init <*> last $ b'
                              in NotHitted (unNotHitted a ++ bs) :- Hitted [b1] :- Empty
                          else NotHitted (unNotHitted a ++ b') :-  Empty
       a' :- Empty -> let b' = unHitted b
                      in if (not.null) b'
                         then let (bs,b1) = (,) <$> init <*> last $ b'
                              in NotHitted (unNotHitted a ++ bs) :- Hitted [b1] :- a' :- Empty
                         else NotHitted (unNotHitted a ++ b' ++ unNotHitted a') :-  Empty 
       a' :- b' :- xs'' -> NotHitted (unNotHitted a ++ unHitted b ++ unNotHitted a') :- b' :- xs''

       
     