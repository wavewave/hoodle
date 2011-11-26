{-# LANGUAGE TypeOperators #-}

module Application.HXournal.Accessor where

import Application.HXournal.Type
import Application.HXournal.Type.Event 
import Application.HXournal.Type.XournalBBox

import Control.Applicative
import Control.Monad
import qualified Control.Monad.State as St

import Control.Monad.Trans
import Control.Category
import Data.Label
import Prelude hiding ((.),id)

import Graphics.UI.Gtk hiding (get,set)

adjustments :: HXournalState :-> (Adjustment,Adjustment) 
adjustments = Lens $ (,) <$> (fst `for` horizAdjustment)
                         <*> (snd `for` vertAdjustment)

getPenType :: Iteratee MyEvent XournalStateIO PenType
getPenType = get (penType.penInfo) <$> lift (St.get)
      
getAllStrokeBBoxInCurrentPage :: Iteratee MyEvent XournalStateIO [StrokeBBox]
getAllStrokeBBoxInCurrentPage = do 
  xstate <- lift St.get 
  let pagenum = get currentPageNum xstate 
      pagebbox = (!!pagenum) . xojbbox_pages . get xournalbbox $ xstate 
  let strs = do 
        l <- pagebbox_layers pagebbox 
        s <- layerbbox_strokes l
        return s 
  return strs 