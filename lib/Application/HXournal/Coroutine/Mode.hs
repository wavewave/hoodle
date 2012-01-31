-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.Coroutine.Mode 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Application.HXournal.Coroutine.Mode where

import Application.HXournal.Type.Event
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.XournalState
import Application.HXournal.Type.PageArrangement
import Application.HXournal.Type.Canvas
import Application.HXournal.View.Coordinate
import Application.HXournal.Accessor
--import Data.Foldable
import Data.Traversable
import Control.Monad.Trans
import Control.Category
import Data.Label
import Data.Xournal.Generic
import Graphics.Xournal.Render.BBoxMapPDF
import Prelude hiding ((.),id, mapM_, mapM)

modeChange :: MyEvent -> MainCoroutine () 
modeChange command = case command of 
                       ToViewAppendMode -> updateXState select2edit
                       ToSelectMode     -> updateXState edit2select 
                       _ -> return ()
  where select2edit xst =  
          either (noaction xst) (whenselect xst) . xojstateEither . get xournalstate $ xst
        edit2select xst = 
          either (whenedit xst) (noaction xst) . xojstateEither . get xournalstate $ xst
        noaction xstate = const (return xstate)
        whenselect xstate txoj = return . flip (set xournalstate) xstate 
                                 . ViewAppendState . GXournal (get g_selectTitle txoj)
                                 =<< liftIO (mapM resetPageBuffers (get g_selectAll txoj))
        whenedit xstate xoj = return . flip (set xournalstate) xstate 
                              . SelectState  
                              $ GSelect (get g_title xoj) (gpages xoj) Nothing



viewModeChange :: MyEvent -> MainCoroutine () 
viewModeChange command = case command of 
                           ToSinglePage -> updateXState cont2single
                           ToContSinglePage -> updateXState single2cont 
                           _ -> return ()
  where cont2single xst =  
          selectBoxAction (noaction xst) (whencont xst) . get currentCanvasInfo $ xst
        single2cont xst = 
          selectBoxAction (whensing xst) (noaction xst) . get currentCanvasInfo $ xst
        noaction xstate = const (return xstate)

        whencont xstate _ = do 
          liftIO $ putStrLn "cont2single"
          return xstate

        whensing xstate cinfo = do 
          liftIO $ putStrLn "single2cont"
          cdim <- liftIO $  return . canvasDim =<< getCanvasGeometry xstate 
          let zmode = get (zoomMode.viewInfo) cinfo
              cpn = PageNum . get currentPageNum $ cinfo 
              arr = makeContinousSingleArrangement zmode cdim (getXournal xstate) cpn
              vinfo = get viewInfo cinfo 
              nvinfo = ViewInfo (get zoomMode vinfo) arr 
              ncinfo = CanvasInfo (get canvasId cinfo)
                                  (get drawArea cinfo)
                                  (get scrolledWindow cinfo)
                                  nvinfo 
                                  (get currentPageNum cinfo)
                                  (get currentPage cinfo)
                                  (get horizAdjustment cinfo)
                                  (get vertAdjustment cinfo)

          return . modifyCurrentCanvasInfo (const (CanvasInfoBox ncinfo)) $ xstate


