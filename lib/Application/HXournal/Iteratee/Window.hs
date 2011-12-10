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

{- newCanvas :: CanvasId -> Iteratee MyEvent XournalStateIO () 
newCanvas cid = do 
  tref getSt -}


horizontalSplit :: Iteratee MyEvent XournalStateIO ()
horizontalSplit = do 
    liftIO $ putStrLn "horizontalSplit"
    xstate <- getSt
    let cmap = get canvasInfoMap xstate
        currcid = get currentCanvas xstate
        newcid = newCanvasId cmap 
        fstate = get frameState xstate
        xojstate = get xournalstate xstate
        enewfstate = splitWindow currcid (newcid,SplitHorizontal) fstate 
    case enewfstate of 
      Left _ -> return ()
      Right fstate' -> do 
        let oldcinfo = case M.lookup currcid cmap of
                         Nothing -> error "noway! in horizontalSplit " 
                         Just c -> c 
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
        liftIO $ putStrLn $ "test2 :" ++ show (M.keys (get canvasInfoMap xstate'))          
        putSt xstate'
        -- liftIO $ threadDelay 1000000
        liftIO $ containerRemove rtcntr rtwin
        -- liftIO $ print fstate 
        win <- liftIO $ constructFrame fstate' cmap'         
        let xstate'' = set rootWindow win xstate'
        liftIO $ putStrLn $ "test:" ++ show (M.keys (get canvasInfoMap xstate''))
        putSt xstate''



        liftIO $ boxPackEnd rtcntr win PackGrow 0 
        liftIO $ widgetShowAll rtcntr 
                     
 --         widgetDestroy rtwin
 --         boxPackEnd rtcntr win PackGrow 0



verticalSplit :: Iteratee MyEvent XournalStateIO ()
verticalSplit = do 
    liftIO $ putStrLn "verticalSplit"

deleteCanvas :: Iteratee MyEvent XournalStateIO ()
deleteCanvas = do 
    liftIO $ putStrLn "deleteCanvas"  
