{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Window 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Window where

import           Control.Category
import           Control.Lens
import           Control.Monad.State 
import qualified Data.IntMap as M
import           Data.Maybe
-- import           Data.Time.Clock 
import           Graphics.UI.Gtk hiding (get,set)
--
import           Data.Hoodle.Generic
import           Data.Hoodle.Simple (Dimension(..))
--
import           Hoodle.Accessor
import           Hoodle.Coroutine.Draw
import           Hoodle.Coroutine.Page
import           Hoodle.ModelAction.Page
import           Hoodle.ModelAction.Window
import           Hoodle.Type.Canvas
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Event
import           Hoodle.Type.HoodleState
import           Hoodle.Type.PageArrangement
-- import           Hoodle.Type.Predefined
import           Hoodle.Type.Window
import           Hoodle.Util
import           Hoodle.View.Draw
--
import Prelude hiding ((.),id)

-- | canvas configure with general zoom update func
canvasConfigureGenUpdate :: MainCoroutine () 
                            -> CanvasId 
                            -> CanvasDimension 
                            -> MainCoroutine () 
canvasConfigureGenUpdate updatefunc cid cdim 
  = (updateXState $ selectBoxAction fsingle fcont . getCanvasInfo cid )
    >> updatefunc 
  where fsingle cinfo = do 
          xstate <- get 
          cinfo' <- liftIO $ updateCanvasDimForSingle cdim cinfo 
          return $ setCanvasInfo (cid,CanvasSinglePage cinfo') xstate
        fcont cinfo = do 
          xstate <- get
          page <- getCurrentPageCvsId cid
          let pdim = PageDimension (view gdimension page)
          cinfo' <- liftIO $ updateCanvasDimForContSingle pdim cdim cinfo 
          return $ setCanvasInfo (cid,CanvasContPage cinfo') xstate 
  
-- | 
doCanvasConfigure :: CanvasId 
                     -> CanvasDimension 
                     -> MainCoroutine () 
doCanvasConfigure = canvasConfigureGenUpdate canvasZoomUpdateAll

-- | 
eitherSplit :: SplitType -> MainCoroutine () 
eitherSplit stype = do
    xstate <- get
    let cmap = getCanvasInfoMap xstate
        currcid = getCurrentCanvasId xstate
        newcid = newCanvasId cmap 
        fstate = view frameState xstate
        enewfstate = splitWindow currcid (newcid,stype) fstate 
    case enewfstate of 
      Left _ -> return ()
      Right fstate' -> do 
        cinfobox <- maybeError "eitherSplit" . M.lookup currcid $ cmap 
        let rtwin = view rootWindow xstate
            rtcntr = view rootContainer xstate 
            rtrwin = view rootOfRootWindow xstate 
        liftIO $ containerRemove rtcntr rtwin
        (xstate'',win,fstate'') <- 
          liftIO $ constructFrame' cinfobox xstate fstate'
        let xstate3 = set frameState fstate'' 
                      . set rootWindow win 
                      $ xstate''
        put xstate3 
        liftIO $ boxPackEnd rtcntr win PackGrow 0 
        liftIO $ widgetShowAll rtcntr  
        -- liftIO $ widgetShowAll win 
        liftIO $ widgetQueueDraw rtrwin
        (xstate4,_wconf) <- liftIO $ eventConnect xstate3 (view frameState xstate3)
        xstate5 <- liftIO $ updatePageAll (view hoodleModeState xstate4) xstate4
        put xstate5 
        canvasZoomUpdateAll
        invalidateAll 

-- | 
deleteCanvas :: MainCoroutine () 
deleteCanvas = do 
    xstate <- get
    let cmap = getCanvasInfoMap xstate
        currcid = getCurrentCanvasId xstate
        fstate = view frameState xstate
        enewfstate = removeWindow currcid fstate 
    case enewfstate of 
      Left _ -> return ()
      Right Nothing -> return ()
      Right (Just fstate') -> do 
        let cmap' = M.delete currcid cmap
            newcurrcid = maximum (M.keys cmap')
        xstate0 <- changeCurrentCanvasId newcurrcid 
        let xstate1 = maybe xstate0 id $ setCanvasInfoMap cmap' xstate0
        put xstate1
        let rtwin = view rootWindow xstate1
            rtcntr = view rootContainer xstate1 
            rtrwin = view rootOfRootWindow xstate1 
        liftIO $ containerRemove rtcntr rtwin
        (xstate'',win,fstate'') <- liftIO $ constructFrame xstate1 fstate'
        let xstate3 = set frameState fstate'' 
                      . set rootWindow win 
                      $ xstate''
        put xstate3
        liftIO $ boxPackEnd rtcntr win PackGrow 0 
        liftIO $ widgetShowAll rtcntr  
        liftIO $ widgetQueueDraw rtrwin
        (xstate4,_wconf) <- liftIO $ eventConnect xstate3 (view frameState xstate3)
        canvasZoomUpdateAll
        xstate5 <- liftIO $ updatePageAll (view hoodleModeState xstate4) xstate4
        put xstate5 
        invalidateAll 

            
-- | 
paneMoveStart :: MainCoroutine () 
paneMoveStart = do 
    ev <- nextevent 
    case ev of 
      UpdateCanvas cid -> invalidateInBBox Nothing Efficient cid >> paneMoveStart 
      PaneMoveEnd -> do 
        return () 
      CanvasConfigure cid w' h'-> do 
        canvasConfigureGenUpdate canvasZoomUpdateBufAll cid (CanvasDimension (Dim w' h')) 
        >> paneMoveStart
      _ -> paneMoveStart
       

-- | not yet implemented?
paneMoved :: MainCoroutine () 
paneMoved = do 
  liftIO $ putStrLn "pane moved called"
  
  

