module Application.HXournal.Iteratee.Draw where

import Graphics.UI.Gtk

import Application.HXournal.Type.Event
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.XournalState
import Application.HXournal.Type.XournalBBox

import Text.Xournal.Type 

import Control.Applicative 
import Application.HXournal.Draw
import Control.Monad.Trans

import qualified Control.Monad.State as St

invalidate :: Iteratee MyEvent XournalStateIO () 
invalidate = do 
  xstate <- lift St.get  
  liftIO (updateCanvas <$> get drawArea 
                       <*> get xournalbbox 
                       <*> get currentPageNum 
                       <*> get viewInfo 
                       $ xstate )

invalidateBBox :: BBox -> Iteratee MyEvent XournalStateIO () 
invalidateBBox bbox = do 
  xstate <- lift St.get  
  let pagenum = get currentPageNum xstate 
  let page = (!!pagenum) . xournalPages . get xournalbbox $ xstate
  liftIO (updateCanvasBBox <$> get drawArea 
                           <*> pure page 
                           <*> get viewInfo 
                           <*> pure bbox
                           $ xstate )
