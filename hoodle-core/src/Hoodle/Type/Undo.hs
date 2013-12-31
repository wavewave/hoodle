-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Type.Undo 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Type.Undo where

import Data.Hoodle.Zipper 

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
