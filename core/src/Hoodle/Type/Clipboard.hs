module Hoodle.Type.Clipboard where

import Data.Hoodle.BBox (BBoxed)
import Data.Hoodle.Simple (Stroke)

-- |
newtype Clipboard = Clipboard {unClipboard :: [BBoxed Stroke]}

-- |
emptyClipboard :: Clipboard
emptyClipboard = Clipboard []

-- |
isEmpty :: Clipboard -> Bool
isEmpty = null . unClipboard

-- |
getClipContents :: Clipboard -> [BBoxed Stroke]
getClipContents = unClipboard

-- |
replaceClipContents :: [BBoxed Stroke] -> Clipboard -> Clipboard
replaceClipContents strs _ = Clipboard strs
