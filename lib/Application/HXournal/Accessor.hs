module Application.HXournal.Accessor where

import Application.HXournal.Type
import Application.HXournal.Coroutine

import Control.Applicative
import Control.Monad
import qualified Control.Monad.State as S

import Control.Monad.Trans

import Control.Category
import Data.Label
import Prelude hiding ((.),id)

import Application.HXournal.Type.Event 

getPenType :: Iteratee MyEvent XournalStateIO PenType
getPenType = get (penType.penInfo) <$> lift (S.get)
             -- lift (pm_pentype . penMode <$> get)
