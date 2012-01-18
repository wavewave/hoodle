
-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.Type.Undo 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
module Application.HXournal.Type.Undo where

import Data.Sequence
import Data.Xournal.Select

{-
type SeqZipper a = (a, (Seq a,Seq a))

                 

singletonSZ :: a -> SeqZipper a  
singletonSZ x = (x, (empty,empty))

appendGoLast :: SeqZipper a -> a -> SeqZipper a
appendGoLast (y,(y1s,y2s)) x = (x, ((y1s |> y) >< y2s, empty))

chopFirst :: SeqZipper a -> Maybe (SeqZipper a)
chopFirst (y,(y1s,y2s)) = 
  case viewl y1s of
    EmptyL -> case viewl y2s of 
                EmptyL -> Nothing 
                z :< zs -> Just (z,(empty,zs))
    z :< zs -> Just (y,(zs,y2s))
    
moveLeft :: SeqZipper a -> Maybe (SeqZipper a)
moveLeft (x,(x1s,x2s)) = 
  case viewr x1s of
    EmptyR -> Nothing 
    zs :> z -> Just (z,(zs,x<|x2s))

moveRight :: SeqZipper a -> Maybe (SeqZipper a) 
moveRight (x,(x1s,x2s)) = 
  case viewl x2s of 
    EmptyL -> Nothing
    z :< zs -> Just (z,(x1s|>x,zs)) 


current :: SeqZipper a -> a 
current (x,(_,_)) = x

prev :: SeqZipper a -> Maybe a 
prev = fmap current . moveLeft

next :: SeqZipper a -> Maybe a 
next = fmap current . moveRight
-}

data UndoTable a = UndoTable { undo_allowednum :: Int
                             , undo_totalnum :: Int 
                             , undo_zipper :: Maybe (SeqZipper a)
                             }
 
emptyUndo :: Int -> UndoTable a
emptyUndo n | n > 0 = UndoTable n 0 Nothing
            | otherwise = error "undo table must be larger than 0" 

singletonUndo :: Int -> a -> UndoTable a 
singletonUndo n e = addToUndo (emptyUndo n) e

addToUndo :: UndoTable a -> a -> UndoTable a 
addToUndo utable e = 
  let tn = undo_totalnum utable 
      an = undo_allowednum utable 
      mzs = undo_zipper utable 
  in case mzs of  
       Nothing -> UndoTable an 1 . Just . singletonSZ $ e
       Just zs -> 
         if tn < an
           then UndoTable an (tn+1) . Just . appendGoLast zs $ e
           else UndoTable an an . chopFirst . appendGoLast zs $ e

getCurrentUndoItem :: UndoTable a -> Maybe a
getCurrentUndoItem = fmap current . undo_zipper

getPrevUndo :: UndoTable a -> Maybe (a, UndoTable a)
getPrevUndo t = do 
  newzs <- moveLeft =<< undo_zipper t 
  return (current newzs, t {undo_zipper = Just newzs})
  
getNextUndo :: UndoTable a -> Maybe (a, UndoTable a)
getNextUndo t = do 
  newzs <- moveRight =<< undo_zipper t 
  return (current newzs, t {undo_zipper = Just newzs})

numOfUndo :: UndoTable a -> Int 
numOfUndo = undo_totalnum 
