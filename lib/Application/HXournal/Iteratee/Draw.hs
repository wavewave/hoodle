module Application.HXournal.Iteratee.Draw where

import Graphics.UI.Gtk hiding (get,set)

import Application.HXournal.Type.Event
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.XournalState

import Application.HXournal.Draw
import Application.HXournal.Accessor

import Graphics.Xournal.Type
import Graphics.Xournal.Type.Select
import Graphics.Xournal.Render.BBox

import Text.Xournal.Type 

import Control.Applicative 

import Control.Monad
import Control.Monad.Trans

import qualified Control.Monad.State as St
import qualified Data.IntMap as M

import Control.Category
import Data.Label
import Prelude hiding ((.),id)

invalidateSelSingle :: CanvasId -> Maybe BBox 
                       -> PageDrawF
                       -> PageDrawFSel 
                       -> Iteratee MyEvent XournalStateIO ()
invalidateSelSingle cid mbbox drawf drawfsel = do
  xstate <- lift St.get  
  let  maybeCvs = M.lookup cid (get canvasInfoMap xstate)
  case maybeCvs of 
    Nothing -> return ()
    Just cvsInfo -> do 
      let -- pagenum = get currentPageNum cvsInfo
          -- xojst = get xournalstate xstate
          page = case get currentPage cvsInfo of  
                   -- Right pgselect -> pageBBoxFromPageSelect pgselect
                   Right _ -> error "not yet implemented in invalidateSelSingle"
                   Left pg -> pg 
      liftIO (drawf <$> get drawArea 
                    <*> pure page 
                    <*> get viewInfo 
                    <*> pure mbbox
                    $ cvsInfo )

invalidateGenSingle :: CanvasId -> Maybe BBox -> PageDrawF
                    -> Iteratee MyEvent XournalStateIO ()
invalidateGenSingle cid mbbox drawf = do
  xstate <- lift St.get  
  let  maybeCvs = M.lookup cid (get canvasInfoMap xstate)
  case maybeCvs of 
    Nothing -> return ()
    Just cvsInfo -> do 
      -- let pagenum = get currentPageNum cvsInfo
      let page = case get currentPage cvsInfo of
                   -- Right pgselect -> pageBBoxFromPageSelect pgselect
                   Right _ -> error "no invalidateGenSingle implementation yet"
                   Left pg -> pg
      liftIO (drawf <$> get drawArea 
                    <*> pure page 
                    <*> get viewInfo 
                    <*> pure mbbox
                    $ cvsInfo )


invalidateGen  :: [CanvasId] -> Maybe BBox -> PageDrawF
               -> Iteratee MyEvent XournalStateIO ()
invalidateGen cids mbbox drawf = do                
  forM_ cids $ \x -> invalidateSelSingle x mbbox drawf dummyDraw

invalidateAll :: Iteratee MyEvent XournalStateIO ()
invalidateAll = do
  xstate <- getSt
  let cinfoMap  = get canvasInfoMap xstate
      keys = M.keys cinfoMap 
  invalidateGen keys Nothing drawPageInBBox

invalidateOther :: Iteratee MyEvent XournalStateIO ()
invalidateOther = do 
  xstate <- getSt
  let currCvsId = get currentCanvas xstate
      cinfoMap  = get canvasInfoMap xstate
      keys = M.keys cinfoMap 
  invalidateGen (filter (/=currCvsId) keys) Nothing drawPageInBBox

invalidate :: CanvasId -> Iteratee MyEvent XournalStateIO () 
invalidate cid = invalidateGenSingle cid Nothing drawPageInBBox 

invalidateInBBox :: CanvasId -> BBox -> Iteratee MyEvent XournalStateIO ()
invalidateInBBox cid bbox = invalidateGenSingle cid (Just bbox) drawPageInBBox

invalidateDrawBBox :: CanvasId -> BBox -> Iteratee MyEvent XournalStateIO () 
invalidateDrawBBox cid bbox = invalidateGenSingle cid (Just bbox) drawBBox

invalidateBBoxOnly :: CanvasId -> Iteratee MyEvent XournalStateIO () 
invalidateBBoxOnly cid = invalidateGenSingle cid Nothing drawBBoxOnly

