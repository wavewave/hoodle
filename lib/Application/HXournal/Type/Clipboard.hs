{-# LANGUAGE TemplateHaskell #-}

module Application.HXournal.Type.Clipboard 
--       ( Clipboard
--       , emptyClipboard
--       , isEmpty
--       , getClipContents 
--       , replaceClipContents
--       ) 
  where

import Control.Category
import Data.Label 
import Prelude hiding ((.), id)

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

data SelectType = SelectRegionWork 
                | SelectRectangleWork 
                | SelectVerticalSpaceWork
                | SelectHandToolWork 
                deriving (Show,Eq) 

data SelectInfo = SelectInfo { _selectType :: SelectType
                             }
             deriving (Show) 

$(mkLabels [''SelectInfo])