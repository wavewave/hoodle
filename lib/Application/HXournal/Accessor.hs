module Application.HXournal.Accessor where

import Application.HXournal.Type
import Application.HXournal.Coroutine

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans

getPenType :: Iteratee MyEvent XournalStateIO PenType
getPenType = lift (pm_pentype . penMode <$> get)
