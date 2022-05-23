-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : Data.Hoodle.Util
-- Copyright   : (c) 2011, 2012, 2015 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
module Data.Hoodle.Util where

-- |
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

-- |
snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

-- |
trd3 :: (a, b, c) -> c
trd3 (_, _, z) = z
