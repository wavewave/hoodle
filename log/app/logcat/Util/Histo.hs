{-# LANGUAGE BangPatterns #-}

module Util.Histo
  ( aggregateCount,
    histoAdd,
  )
where

import Data.List (group, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

aggregateCount :: [String] -> [(String, Int)]
aggregateCount = mapMaybe count . group . sort
  where
    count ys@(x : _) = Just (x, length ys)
    count [] = Nothing

histoAdd :: Map String Int -> (String, Int) -> Map String Int
histoAdd !hist (key, value) = Map.alter upd key hist
  where
    upd Nothing = Just value
    upd (Just value0) = Just (value0 + value)
