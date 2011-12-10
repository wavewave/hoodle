module Application.HXournal.Iteratee.Window where

import Application.HXournal.Type.Event
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.Window
import Application.HXournal.Type.XournalState
import Application.HXournal.Type.Coroutine

import Control.Monad.Trans
-- import Control.Concurrent 

import Application.HXournal.ModelAction.Window
import Application.HXournal.Accessor
import Application.HXournal.ModelAction.Page

import Control.Category
import Data.Label
import Prelude hiding ((.),id)

import Graphics.UI.Gtk hiding (get,set)
import qualified Data.IntMap as M

eitherSplit :: SplitType -> Iteratee MyEvent XournalStateIO ()
eitherSplit stype = do
    xstate <- getSt
    let cmap = get canvasInfoMap xstate
        currcid = get currentCanvas xstate
        newcid = newCanvasId cmap 
        fstate = get frameState xstate
        xojstate = get xournalstate xstate
        enewfstate = splitWindow currcid (newcid,stype) fstate 
    case enewfstate of 
      Left _ -> return ()
      Right fstate' -> do 
        let oldcinfo = case M.lookup currcid cmap of
                         Nothing -> error "noway! in eitherSplit " 
                         Just c -> c 
        liftIO $ removePanes fstate 
        cinfo <- liftIO $ initCanvasInfo xstate newcid 
        let cinfo' = set viewInfo (get viewInfo oldcinfo)
                     . set currentPageNum (get currentPageNum oldcinfo)
                     . set currentPage (get currentPage oldcinfo)
                     $ cinfo
        let cmap' = M.insert newcid cinfo' cmap
        liftIO $ putStrLn $ "in window " ++ show (M.keys cmap')
        let rtwin = get rootWindow xstate
            rtcntr = get rootContainer xstate 
            xstate' = set canvasInfoMap cmap'
                      . set frameState fstate'
                      $ xstate
        putSt xstate'
        liftIO $ containerRemove rtcntr rtwin

        (win,fstate'') <- liftIO $ constructFrame fstate' cmap'         
        let xstate'' = set frameState fstate'' 
                       . set rootWindow win $ xstate'
        putSt xstate''
        liftIO $ boxPackEnd rtcntr win PackGrow 0 
        liftIO $ widgetShowAll rtcntr 


deleteCanvas :: Iteratee MyEvent XournalStateIO ()
deleteCanvas = do 
    liftIO $ putStrLn "deleteCanvas"  
