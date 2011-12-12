{-# LANGUAGE TypeOperators #-}

module Application.HXournal.Accessor where

import Application.HXournal.Type
import Application.HXournal.Draw 
import Application.HXournal.ModelAction.Page
import Text.Xournal.Type
import Graphics.Xournal.Type 
import Control.Applicative
import Control.Monad
import qualified Control.Monad.State as St
import Control.Monad.Trans
import Control.Category
import qualified Data.IntMap as M
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
  xstate <- getSt 
  let currCvsInfo  = getCurrentCanvasInfo xstate 
  let pagenum = get currentPageNum currCvsInfo
      pagebbox = getPage currCvsInfo
      strs = do 
        l <- pageLayers pagebbox 
        s <- layerbbox_strokes l
        return s 
  return strs 
      
      
updateCanvasInfo :: CanvasInfo -> HXournalState -> HXournalState
updateCanvasInfo cinfo xstate = 
  let cid = get canvasId cinfo
      cmap = get canvasInfoMap xstate
      cmap' = M.adjust (const cinfo) cid cmap 
      xstate' = set canvasInfoMap cmap' xstate
  in xstate' 
 
otherCanvas :: HXournalState -> [Int] 
otherCanvas = M.keys . get canvasInfoMap 

changeCurrentCanvasId :: CanvasId -> Iteratee MyEvent XournalStateIO HXournalState
changeCurrentCanvasId cid = do xstate1 <- getSt 
                               let xstate = set currentCanvas cid xstate1
                               putSt xstate
                               return xstate
                               
getCanvasInfo :: CanvasId -> HXournalState -> CanvasInfo 
getCanvasInfo cid xstate = 
  let cinfoMap = get canvasInfoMap xstate
      maybeCvs = M.lookup cid cinfoMap
  in  case maybeCvs of 
        Nothing -> error $ "no canvas with id = " ++ show cid 
        Just cvsInfo -> cvsInfo

getCurrentCanvasInfo :: HXournalState -> CanvasInfo 
getCurrentCanvasInfo xstate = getCanvasInfo (get currentCanvas xstate) xstate
      

getCanvasGeometry :: CanvasInfo -> Iteratee MyEvent XournalStateIO CanvasPageGeometry
getCanvasGeometry cinfo = do 
    let canvas = get drawArea cinfo
        page = getPage cinfo
        zmode = get (zoomMode.viewInfo) cinfo
        (x0,y0) = get (viewPortOrigin.viewInfo) cinfo
    liftIO (getCanvasPageGeometry canvas page (x0,y0))