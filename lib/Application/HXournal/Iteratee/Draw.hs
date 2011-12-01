module Application.HXournal.Iteratee.Draw where

import Graphics.UI.Gtk hiding (get,set)

import Application.HXournal.Type.Event
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.XournalState

import Application.HXournal.Draw
import Application.HXournal.Accessor

import Graphics.Xournal.Type
import Graphics.Xournal.Render.BBox

import Text.Xournal.Type 

import Control.Applicative 

import Control.Monad.Trans

import qualified Control.Monad.State as St
import qualified Data.Map as M

import Control.Category
import Data.Label
import Prelude hiding ((.),id)

invalidateAll :: Iteratee MyEvent XournalStateIO ()
invalidateAll = do 
  xstate <- getSt
  let cinfoMap  = get canvasInfoMap xstate
      keys = M.keys cinfoMap 
  mapM_ invalidate keys

invalidateOther :: Iteratee MyEvent XournalStateIO ()
invalidateOther = do 
  xstate <- getSt
  let currCvsId = get currentCanvas xstate
      cinfoMap  = get canvasInfoMap xstate
      keys = M.keys cinfoMap 
  mapM_ invalidate (filter (/=currCvsId) keys)


invalidate :: CanvasId -> Iteratee MyEvent XournalStateIO () 
invalidate cid = do 
  xstate <- lift St.get  
  let -- currCvsId = get currentCanvas xstate
      maybeCvs = M.lookup cid (get canvasInfoMap xstate)
  case maybeCvs of 
    Nothing -> return ()
    Just cvsInfo -> do 
      let xojbbox = get xournalbbox xstate
      liftIO (updateCanvas <$> get drawArea 
                           <*> pure xojbbox
                           <*> get currentPageNum 
                           <*> get viewInfo 
                           $ cvsInfo )

invalidateInBBox :: CanvasId -> BBox -> Iteratee MyEvent XournalStateIO ()
invalidateInBBox cid bbox = do 
  xstate <- lift St.get  
  let  maybeCvs = M.lookup cid (get canvasInfoMap xstate)
  case maybeCvs of 
    Nothing -> return ()
    Just cvsInfo -> do 
      let pagenum = get currentPageNum cvsInfo
      let page = (!!pagenum) . xournalPages . get xournalbbox $ xstate
      liftIO (updateCanvasInBBox <$> get drawArea 
                                 <*> pure page 
                                 <*> get viewInfo 
                                 <*> pure bbox
                                 $ cvsInfo )

  
invalidateDrawBBox :: CanvasId -> BBox -> Iteratee MyEvent XournalStateIO () 
invalidateDrawBBox cid bbox = do 
  xstate <- lift St.get  
  let  maybeCvs = M.lookup cid (get canvasInfoMap xstate)
  case maybeCvs of 
    Nothing -> return ()
    Just cvsInfo -> do 
      let pagenum = get currentPageNum cvsInfo
      let page = (!!pagenum) . xournalPages . get xournalbbox $ xstate
      liftIO (drawBBox <$> get drawArea 
                       <*> pure page 
                       <*> get viewInfo 
                       <*> pure bbox
                       $ cvsInfo )


invalidateBBoxOnly :: CanvasId -> Iteratee MyEvent XournalStateIO () 
invalidateBBoxOnly cid = do 
  xstate <- lift St.get  
  let  maybeCvs = M.lookup cid (get canvasInfoMap xstate)
  case maybeCvs of 
    Nothing -> return ()
    Just cvsInfo -> do 
      let xojbbox = get xournalbbox xstate
      liftIO (updateCanvasBBoxOnly <$> get drawArea 
                                   <*> pure xojbbox
                                   <*> get currentPageNum 
                                   <*> get viewInfo 
                                   $ cvsInfo )
