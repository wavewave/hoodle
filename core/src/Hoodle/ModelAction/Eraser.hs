-- |
-- Module      : Hoodle.ModelAction.Eraser
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
module Hoodle.ModelAction.Eraser where

import Control.Monad.State (State)
import Data.Hoodle.BBox
  ( BBox,
    GetBBoxable,
  )
import Graphics.Hoodle.Render.Type.HitTest
  ( AlterList (..),
    Hitted (..),
    NotHitted (..),
  )
import Graphics.Hoodle.Render.Util.HitTest (elimHitted)

-- |
eraseHitted ::
  (GetBBoxable a) =>
  AlterList (NotHitted a) (AlterList (NotHitted a) (Hitted a)) ->
  State (Maybe BBox) [a]
eraseHitted Empty = error "something wrong in eraseHitted"
eraseHitted (n :- Empty) = return (unNotHitted n)
eraseHitted (n :- h :- rest) = do
  mid <- elimHitted h
  (unNotHitted n ++) . (mid ++) <$> eraseHitted rest
