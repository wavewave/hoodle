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
import Application.HXournal.ModelAction.Page
import Application.HXournal.Coroutine.Page
import Application.HXournal.Coroutine.Draw
import Application.HXournal.Accessor
import Control.Category
import Data.Label
import Graphics.UI.Gtk hiding (get,set)
import Graphics.Rendering.Cairo
import qualified Data.IntMap as M
import Data.Maybe
import Data.Xournal.Simple (Dimension(..))
import Prelude hiding ((.),id)

-- | 

canvasConfigure :: CanvasId -> CanvasDimension -> MainCoroutine () 
canvasConfigure cid cdim@(CanvasDimension (Dim w' h')) = do 
    xstate <- getSt 
    let cinfobox = getCanvasInfo cid xstate
    xstate' <- selectBoxAction (fsingle xstate) (fcont xstate) cinfobox
    putSt xstate'
    canvasZoomUpdateAll 
  where cdim = CanvasDimension (Dim w' h')
        fsingle :: HXournalState -> CanvasInfo SinglePage -> MainCoroutine HXournalState
        fsingle xstate cinfo = do 
          let cinfo' = updateCanvasDimForSingle cdim cinfo 
          return $ setCanvasInfo (cid,CanvasInfoBox cinfo') xstate
        fcont xstate cinfo = do 
          let cinfo' = updateCanvasDimForContSingle cdim cinfo 
          return $ setCanvasInfo (cid,CanvasInfoBox cinfo') xstate 


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
        case maybeError "eitherSplit" . M.lookup currcid $ cmap of 
          CanvasInfoBox oldcinfo -> do 
            liftIO $ putStrLn "called here"
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
            -- liftIO $ putStrLn " called here 2 " 
            -- canvasZoomUpdateAll
            -- liftIO $ putStrLn " called here 3 "
            xstate5 <- liftIO $ updatePageAll (get xournalstate xstate4) xstate4
            putSt xstate5 
            invalidateAll 

          


-- | 

deleteCanvas :: MainCoroutine () 
deleteCanvas = do 
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
            xstate0 <- changeCurrentCanvasId newcurrcid 
            let xstate1 = set canvasInfoMap cmap' xstate0
            putSt xstate1
            let rtwin = get rootWindow xstate1
                rtcntr = get rootContainer xstate1 
            liftIO $ containerRemove rtcntr rtwin
            (xstate'',win,fstate'') <- liftIO $ constructFrame xstate1 fstate'
            let xstate3 = set frameState fstate'' 
                            . set rootWindow win 
                            $ xstate''
            putSt xstate3
            liftIO $ boxPackEnd rtcntr win PackGrow 0 
            liftIO $ widgetShowAll rtcntr  
            (xstate4,wconf) <- liftIO $ eventConnect xstate3 (get frameState xstate3)
            canvasZoomUpdateAll
            xstate5 <- liftIO $ updatePageAll (get xournalstate xstate4) xstate4
            putSt xstate5 
            invalidateAll 
            
