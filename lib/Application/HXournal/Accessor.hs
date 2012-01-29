{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.Accessor 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--

module Application.HXournal.Accessor where

import Application.HXournal.Type
import Application.HXournal.View.Draw 
import Application.HXournal.ModelAction.Page
import Control.Applicative
import Control.Monad
import qualified Control.Monad.State as St
import Control.Monad.Trans
import Control.Category
import qualified Data.IntMap as M
import Data.Label
import Prelude hiding ((.),id)
import Graphics.UI.Gtk hiding (get,set)
import Data.Xournal.BBox
import Data.Xournal.Generic
import Application.HXournal.Util
import Application.HXournal.ModelAction.Layer 
import Application.HXournal.Type.PageArrangement


getSt :: MainCoroutine HXournalState 
getSt = lift St.get

putSt :: HXournalState -> MainCoroutine () 
putSt = lift . St.put


adjustments :: CanvasInfo a :-> (Adjustment,Adjustment) 
adjustments = Lens $ (,) <$> (fst `for` horizAdjustment)
                         <*> (snd `for` vertAdjustment)

getPenType :: Iteratee MyEvent XournalStateIO PenType
getPenType = get (penType.penInfo) <$> lift (St.get)
      
getAllStrokeBBoxInCurrentPage :: MainCoroutine [StrokeBBox] 
getAllStrokeBBoxInCurrentPage = do 
  xstate <- getSt 
  case get currentCanvas xstate of
    (_,CanvasInfoBox currCvsInfo) -> 
      let pagebbox = getPage currCvsInfo
      in  return [s| l <- gToList (get g_layers pagebbox), s <- get g_bstrokes l ]
  

getAllStrokeBBoxInCurrentLayer :: MainCoroutine [StrokeBBox] 
getAllStrokeBBoxInCurrentLayer = do 
  xstate <- getSt 
  case get currentCanvas xstate of 
    (_,CanvasInfoBox currCvsInfo) -> do 
      let pagebbox = getPage currCvsInfo
          (mcurrlayer, _currpage) = getCurrentLayerOrSet pagebbox
          currlayer = maybe (error "getAllStrokeBBoxInCurrentLayer") id mcurrlayer
      return (get g_bstrokes currlayer)
      
otherCanvas :: HXournalState -> [Int] 
otherCanvas = M.keys . get canvasInfoMap 

changeCurrentCanvasId :: CanvasId -> MainCoroutine HXournalState 
changeCurrentCanvasId cid = do 
    xstate1 <- getSt
    (>>=) (return . M.lookup cid . get canvasInfoMap $ xstate1) $
     maybe (return xstate1) 
           (\cinfo -> let nst = set currentCanvas (cid,cinfo) xstate1 in (putSt nst >> return nst))


getCanvasInfo :: CanvasId -> HXournalState -> CanvasInfoBox 
getCanvasInfo cid xstate = 
  let cinfoMap = get canvasInfoMap xstate
      maybeCvs = M.lookup cid cinfoMap
  in maybeError ("no canvas with id = " ++ show cid) maybeCvs

getCanvasGeometry :: CanvasInfo SinglePage -> MainCoroutine CanvasPageGeometry 
getCanvasGeometry cinfo = do 
    let canvas = get drawArea cinfo
        page = getPage cinfo
        (x0,y0) = bbox_upperleft . unViewPortBBox . get (viewPortBBox.pageArrangement.viewInfo) $ cinfo
    liftIO (getCanvasPageGeometry canvas page (x0,y0))

updateXState :: (HXournalState -> MainCoroutine HXournalState) -> MainCoroutine ()
updateXState action = putSt =<< action =<< getSt 
