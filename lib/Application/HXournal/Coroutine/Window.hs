{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.Coroutine.Window 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Application.HXournal.Coroutine.Window where

import Application.HXournal.Type.Canvas
import Application.HXournal.Type.Window
import Application.HXournal.Type.XournalState
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.PageArrangement
import Application.HXournal.Util
import Control.Monad.Trans
import Application.HXournal.ModelAction.Window
import Application.HXournal.Accessor
import Control.Category
import Data.Label
import Graphics.UI.Gtk hiding (get,set)
import Graphics.Rendering.Cairo
import qualified Data.IntMap as M
import Data.Maybe
import Prelude hiding ((.),id)

-- | 

eitherSplit :: SplitType -> MainCoroutine () 
eitherSplit stype = do
    xstate <- getSt
    let cmap = get canvasInfoMap xstate
        (currcid,_) = get currentCanvas xstate
        newcid = newCanvasId cmap 
        fstate = get frameState xstate
        enewfstate = splitWindow currcid (newcid,stype) fstate 
    case enewfstate of 
      Left _ -> return ()
      Right fstate' -> do 
        liftIO $ putStrLn "hello"
        liftIO $ putStrLn $ show fstate'
        case maybeError "eitherSplit" . M.lookup currcid $ cmap of 
          CanvasInfoBox oldcinfo -> do 
            let rtwin = get rootWindow xstate
                rtcntr = get rootContainer xstate 
            liftIO $ containerRemove rtcntr rtwin
            (xstate'',win,fstate'') <- 
              liftIO $ constructFrame' (CanvasInfoBox oldcinfo) xstate fstate'
            let xstate3 = set frameState fstate'' 
                            . set rootWindow win 
                            $ xstate''
            putSt xstate3 
            liftIO $ boxPackEnd rtcntr win PackGrow 0 
            liftIO $ widgetShowAll rtcntr  
            (xstate4,wconf) <- liftIO $ eventConnect xstate3 (get frameState xstate3)
            liftIO $ putStrLn "haha"

                


-- | 

deleteCanvas :: MainCoroutine () 
deleteCanvas = do 
    liftIO $ putStrLn "deleteCanvas"  
    xstate <- getSt
    let cmap = get canvasInfoMap xstate
        (currcid,_) = get currentCanvas xstate
        fstate = get frameState xstate
        enewfstate = removeWindow currcid fstate 
    case enewfstate of 
      Left _ -> return ()
      Right Nothing -> return ()
      Right (Just fstate') -> do 
      case maybeError "deleteCanvas" (M.lookup currcid cmap) of
        CanvasInfoBox oldcinfo -> do 
          let cmap' = M.delete currcid cmap
              newcurrcid = maximum (M.keys cmap')
          xstate' <- changeCurrentCanvasId newcurrcid 
          let rtwin = get rootWindow xstate'
              rtcntr = get rootContainer xstate' 
              xstate'' = set canvasInfoMap cmap'
                         . set frameState fstate'
                         $ xstate'
          putSt xstate''
          liftIO $ containerRemove rtcntr rtwin
          (xstate''',win,fstate'') <- liftIO $ constructFrame xstate'' fstate'
          let xstate4 = set frameState fstate'' 
                          . set rootWindow win 
                          $ xstate'''
          putSt xstate4
          liftIO $ boxPackEnd rtcntr win PackGrow 0 
          liftIO $ widgetShowAll rtcntr   
          liftIO $ widgetDestroy (get scrolledWindow oldcinfo)
          liftIO $ widgetDestroy (get drawArea oldcinfo)
        
