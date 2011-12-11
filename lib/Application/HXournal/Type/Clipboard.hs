module Application.HXournal.Type.Clipboard 
       ( Clipboard
       , emptyClipboard
       , isEmpty
       , getClipContents 
       , replaceClipContents
       ) where

import Graphics.Xournal.Type

newtype Clipboard = Clipboard { unClipboard :: [StrokeBBox] }

emptyClipboard :: Clipboard
emptyClipboard = Clipboard []

isEmpty :: Clipboard -> Bool 
isEmpty = null . unClipboard 

getClipContents :: Clipboard -> [StrokeBBox] 
getClipContents = unClipboard

replaceClipContents :: [StrokeBBox] -> Clipboard -> Clipboard
replaceClipContents strs _ = Clipboard strs 
