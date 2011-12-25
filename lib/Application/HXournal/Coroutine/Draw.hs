module Application.HXournal.Coroutine.Draw where

import Application.HXournal.Type.Event
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.XournalState
import Application.HXournal.Draw
import Application.HXournal.Accessor


import Data.Xournal.BBox

import Control.Applicative 
import Control.Monad
import Control.Monad.Trans
import qualified Control.Monad.State as St
import qualified Data.IntMap as M
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
      case get currentPage cvsInfo of 
        Left page ->  liftIO (drawf <$> get drawArea 
                                    <*> pure page 
                                    <*> get viewInfo 
                                    <*> pure mbbox
                                    $ cvsInfo )
        Right tpage -> liftIO (drawfsel <$> get drawArea 
                                        <*> pure tpage
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
      let page = case get currentPage cvsInfo of
                   Right _ -> error "no invalidateGenSingle implementation yet"
                   Left pg -> pg
      liftIO (drawf <$> get drawArea 
                    <*> pure page 
                    <*> get viewInfo 
                    <*> pure mbbox
                    $ cvsInfo )


invalidateAll :: Iteratee MyEvent XournalStateIO ()
invalidateAll = do
  xstate <- getSt
  let cinfoMap  = get canvasInfoMap xstate
      keys = M.keys cinfoMap 
  forM_ keys invalidate 

invalidateOther :: Iteratee MyEvent XournalStateIO ()
invalidateOther = do 
  xstate <- getSt
  let currCvsId = get currentCanvas xstate
      cinfoMap  = get canvasInfoMap xstate
      keys = M.keys cinfoMap 
  mapM_ invalidate (filter (/=currCvsId) keys)
  

-- | invalidate clear 

invalidate :: CanvasId -> Iteratee MyEvent XournalStateIO () 
invalidate cid = invalidateSelSingle cid Nothing drawPageClearly drawPageSelClearly


-- | Drawing objects only in BBox

invalidateInBBox :: CanvasId -> BBox -> Iteratee MyEvent XournalStateIO ()
invalidateInBBox cid bbox = invalidateSelSingle cid (Just bbox) drawPageInBBox drawSelectionInBBox

-- | Drawing BBox

invalidateDrawBBox :: CanvasId -> BBox -> Iteratee MyEvent XournalStateIO () 
invalidateDrawBBox cid bbox = invalidateSelSingle cid (Just bbox) drawBBox drawBBoxSel

-- | Drawing using layer buffer

invalidateWithBuf :: CanvasId -> Iteratee MyEvent XournalStateIO () 
invalidateWithBuf = invalidateWithBufInBBox Nothing
  

-- | Drawing using layer buffer in BBox

invalidateWithBufInBBox :: Maybe BBox -> CanvasId -> Iteratee MyEvent XournalStateIO () 
invalidateWithBufInBBox mbbox cid = invalidateSelSingle cid mbbox drawBuf drawSelectionInBBox
                                    


