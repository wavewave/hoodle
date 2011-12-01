{-# LANGUAGE TypeOperators #-}

module Application.HXournal.Accessor where

import Application.HXournal.Type
import Application.HXournal.Type.Event 

import Graphics.Xournal.Type 
import Graphics.Xournal.Render.BBox

import Control.Applicative
import Control.Monad
import qualified Control.Monad.State as St
import Control.Monad.Trans
import Control.Category

import qualified Data.Map as M
import Data.Label
import Prelude hiding ((.),id)

import Graphics.UI.Gtk hiding (get,set)

getSt :: Iteratee MyEvent XournalStateIO HXournalState
getSt = lift St.get

putSt :: HXournalState -> Iteratee MyEvent XournalStateIO ()
putSt = lift . St.put


adjustments :: CanvasInfo :-> (Adjustment,Adjustment) 
adjustments = Lens $ (,) <$> (fst `for` horizAdjustment)
                         <*> (snd `for` vertAdjustment)

getPenType :: Iteratee MyEvent XournalStateIO PenType
getPenType = get (penType.penInfo) <$> lift (St.get)
      
getAllStrokeBBoxInCurrentPage :: Iteratee MyEvent XournalStateIO [StrokeBBox]
getAllStrokeBBoxInCurrentPage = do 
  xstate <- lift St.get 
  let currCvsId = get currentCanvas xstate 
      maybeCurrCvs = M.lookup currCvsId (get canvasInfoMap xstate)
  case maybeCurrCvs of 
    Nothing -> return [] 
    Just currCvsInfo -> do 
      let pagenum = get currentPageNum currCvsInfo
          pagebbox = (!!pagenum) . xojbbox_pages . get xournalbbox $ xstate 
      let strs = do 
            l <- pagebbox_layers pagebbox 
            s <- layerbbox_strokes l
            return s 
      return strs 
      