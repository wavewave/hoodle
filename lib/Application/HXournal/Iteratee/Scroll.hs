module Application.HXournal.Iteratee.Scroll where

import Graphics.UI.Gtk hiding (get,set)

import Application.HXournal.Type.Event 

import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.XournalState

import Application.HXournal.Iteratee.Draw

import qualified Data.Map as M

import Control.Monad.Trans
import qualified Control.Monad.State as St
import Control.Monad.Coroutine.SuspensionFunctors

import Control.Category
import Data.Label
import Prelude hiding ((.), id)

vscrollStart :: CanvasId -> Iteratee MyEvent XournalStateIO () 
vscrollStart cid = do    
{-    xstate1 <- getSt 
    let xstate = set currentCanvas cid xstate1 
    putSt xstate
    let maybeCvs = M.lookup cid (get canvasInfoMap xstate) 
    case maybeCvs of 
      Nothing -> error "scrollStart wrong"
      Just cvsInfo -> do 
        let currxoj = get xournalbbox xstate        
            canvas = get drawArea cvsInfo   
            pagenum = get currentPageNum cvsInfo
            page = (!!pagenum) . xournalPages $ currxoj
            (x0,y0) = get (viewPortOrigin.viewInfo) cvsInfo
            pinfo = get penInfo xstate
            zmode = get (zoomMode.viewInfo) cvsInfo
        geometry <- liftIO (getCanvasPageGeometry canvas page (x0,y0) ) -}
        liftIO $ putStrLn "vscrollStart"
        vscrollMove cid 
        
vscrollMove :: CanvasId -> Iteratee MyEvent XournalStateIO () 
vscrollMove cid = do    
  liftIO $ putStrLn "vscrollMove"
  ev <- await 
  case ev of
    VScrollBarMoved cid' v -> do 
      xstate <- lift St.get 
      let cinfoMap = get canvasInfoMap xstate
          maybeCvs = M.lookup cid cinfoMap 
      case maybeCvs of 
        Nothing -> return ()
        Just cvsInfo -> do 
          let vm_orig = get (viewPortOrigin.viewInfo) cvsInfo
          let cvsInfo' = set (viewPortOrigin.viewInfo) (fst vm_orig,v)
                         $ cvsInfo 
              cinfoMap' = M.adjust (\_ -> cvsInfo') cid cinfoMap  
              xstate' = set canvasInfoMap cinfoMap' 
                        . set currentCanvas cid
                        $ xstate
          lift . St.put $ xstate'
          invalidateBBoxOnly cid
          vscrollMove cid 
    VScrollBarEnd cid v -> do 
      invalidate cid 
      return ()
      