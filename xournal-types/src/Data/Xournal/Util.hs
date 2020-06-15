module Data.Xournal.Util where

-- |
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

-- |
snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

-- |
trd3 :: (a, b, c) -> c
trd3 (_, _, z) = z
