module Application.HXournal.ModelAction.Common where

import Application.HXournal.Type.XournalState

import Control.Category
import Data.Label 
import Prelude hiding ((.),id)

commitChange :: HXournalState -> HXournalState
commitChange old = set isSaved False old 

