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
import Application.HXournal.Type.Event
import Application.HXournal.Type.Window
import Application.HXournal.Type.XournalState
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.PageArrangement
import Application.HXournal.Type.Predefined
import Application.HXournal.Util
import Control.Monad.Trans
import Application.HXournal.ModelAction.Window
import Application.HXournal.ModelAction.Page
import Application.HXournal.Coroutine.Page
import Application.HXournal.Coroutine.Draw
import Application.HXournal.Accessor
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Category
import Data.Label
import Graphics.UI.Gtk hiding (get,set)
import Graphics.Rendering.Cairo
import qualified Data.IntMap as M
import Data.Maybe
import Data.Time.Clock 
import Data.Xournal.Simple (Dimension(..))
import Data.Xournal.Generic
import Prelude hiding ((.),id)

-- | canvas configure with general zoom update func

canvasConfigureGenUpdate :: MainCoroutine () 
                            -> CanvasId 
                            -> CanvasDimension 
                            -> MainCoroutine () 
canvasConfigureGenUpdate updatefunc cid cdim@(CanvasDimension (Dim w' h')) 
  = (updateXState $ selectBoxAction fsingle fcont . getCanvasInfo cid )
    >> updatefunc 
    -- canvasZoomUpdateAll 
  where cdim = CanvasDimension (Dim w' h')
        fsingle cinfo = do 
          xstate <- getSt 
          let cinfo' = updateCanvasDimForSingle cdim cinfo 
          return $ setCanvasInfo (cid,CanvasInfoBox cinfo') xstate
        fcont cinfo = do 
          xstate <- getSt
          page <- getCurrentPageCvsId cid
          let pdim = PageDimension (get g_dimension page)
          let cinfo' = updateCanvasDimForContSingle pdim cdim cinfo 
          return $ setCanvasInfo (cid,CanvasInfoBox cinfo') xstate 
  
-- | 

doCanvasConfigure :: CanvasId 
                     -> CanvasDimension 
                     -> MainCoroutine () 
doCanvasConfigure = canvasConfigureGenUpdate canvasZoomUpdateAll


    -- fsingle :: CanvasInfo SinglePage -> MainCoroutine HXournalState
    {- xstate <- getSt 
    let cinfobox = getCanvasInfo cid xstate -}
    {- xstate' <- -} 
    -- putSt xstate' 
  

-- | 

canvasConfigure' :: CanvasId -> CanvasDimension -> MainCoroutine () 
canvasConfigure' cid cdim@(CanvasDimension (Dim w' h')) = do 
    xstate <- getSt 
    ctime <- liftIO getCurrentTime 
    maybe defaction (chkaction ctime) (get lastTimeCanvasConfigure xstate) 
  where defaction = do 
          ntime <- liftIO getCurrentTime
          doCanvasConfigure cid cdim          
          updateXState (return . set lastTimeCanvasConfigure (Just ntime))    
        chkaction ctime otime = do  
          let dtime = diffUTCTime ctime otime 
          if dtime > predefinedWinReconfTimeBound
             then defaction 
             else return ()


-- | 

eitherSplit :: SplitType -> MainCoroutine () 
eitherSplit stype = do
    xstate <- getSt
    let cmap = getCanvasInfoMap xstate
        currcid = getCurrentCanvasId xstate
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
            xstate5 <- liftIO $ updatePageAll (get xournalstate xstate4) xstate4
            putSt xstate5 
            canvasZoomUpdateAll
            invalidateAll 


-- | 

deleteCanvas :: MainCoroutine () 
deleteCanvas = do 
    xstate <- getSt
    let cmap = getCanvasInfoMap xstate
        currcid = getCurrentCanvasId xstate
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
            let xstate1 = maybe xstate0 id $ setCanvasInfoMap cmap' xstate0
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
            
-- | 

paneMoveStart :: MainCoroutine () 
paneMoveStart = do 
    ev <- await 
    case ev of 
      UpdateCanvas cid -> invalidateWithBuf cid >> paneMoveStart 
      PaneMoveEnd -> return () 
      CanvasConfigure cid w' h'->  
        canvasConfigureGenUpdate canvasZoomUpdateBufAll cid (CanvasDimension (Dim w' h')) 
        >> paneMoveStart
      _ -> paneMoveStart
       


-- | 

paneMoved :: MainCoroutine () 
paneMoved = do 
  liftIO $ putStrLn "pane moved called"
  
  

