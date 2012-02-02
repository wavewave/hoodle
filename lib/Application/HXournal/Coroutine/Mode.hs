{-# LANGUAGE GADTs #-}

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
import Application.HXournal.Type.Alias
import Application.HXournal.Type.PageArrangement
import Application.HXournal.Type.Canvas
import Application.HXournal.View.Coordinate
import Application.HXournal.Accessor
import Application.HXournal.ModelAction.Page
import Application.HXournal.Coroutine.Scroll
--import Data.Foldable
import Data.Traversable
import Control.Applicative
import Control.Monad.Trans
import Control.Category
import Data.Label
import Data.Xournal.Simple (Dimension(..))
import Data.Xournal.Generic
import Graphics.Xournal.Render.BBoxMapPDF
import Graphics.UI.Gtk (adjustmentSetUpper,adjustmentGetValue,adjustmentSetValue)
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
        noaction :: HXournalState -> a -> MainCoroutine HXournalState
        noaction xstate = const (return xstate)
        whenselect :: HXournalState -> Xournal SelectMode -> MainCoroutine HXournalState
        whenselect xstate txoj = return . flip (set xournalstate) xstate 
                                 . ViewAppendState . GXournal (get g_selectTitle txoj)
                                 =<< liftIO (mapM resetPageBuffers (get g_selectAll txoj)) 
        whenedit :: HXournalState -> Xournal EditMode -> MainCoroutine HXournalState   
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
        noaction :: HXournalState -> a -> MainCoroutine HXournalState  
        noaction xstate = const (return xstate)

        whencont xstate _ = do 
          liftIO $ putStrLn "cont2single"
          return xstate

        whensing xstate cinfo = do 
          liftIO $ putStrLn "single2cont"
          cdim <- liftIO $  return . canvasDim =<< getCanvasGeometry xstate 
          let zmode = get (zoomMode.viewInfo) cinfo
              canvas = get drawArea cinfo 
              cpn = PageNum . get currentPageNum $ cinfo 
              page = getPage cinfo
              (hadj,vadj) = get adjustments cinfo 
          (xpos,ypos) <- liftIO $ (,) <$> adjustmentGetValue hadj <*> adjustmentGetValue vadj

          let arr = makeContinuousSingleArrangement zmode cdim (getXournal xstate) 
                                                    (cpn, PageCoord (xpos,ypos))
              ContinuousSingleArrangement (DesktopDimension (Dim w h)) _ _ = arr  
          geometry <- liftIO $ makeCanvasGeometry EditMode (cpn,page) arr canvas
          let DeskCoord (nxpos,nypos) = page2Desktop geometry (cpn,PageCoord (xpos,ypos))
          adjustScrollbarWithGeometryCurrent     
          {- 
          liftIO $ adjustmentSetUpper hadj w 
          liftIO $ adjustmentSetUpper vadj h 
          liftIO $ adjustmentSetValue hadj nxpos
          liftIO $ adjustmentSetValue vadj nypos  -}
          let vinfo = get viewInfo cinfo 
              nvinfo = ViewInfo (get zoomMode vinfo) arr 
              ncinfotemp = CanvasInfo (get canvasId cinfo)
                                      (get drawArea cinfo)
                                      (get scrolledWindow cinfo)
                                      nvinfo 
                                      (get currentPageNum cinfo)
                                      (get currentPage cinfo)
                                      hadj 
                                      vadj 
              ncpn = maybe cpn fst $ desktop2Page geometry (DeskCoord (nxpos,nypos))
              ncinfo = modify currentPageNum (const (unPageNum ncpn)) ncinfotemp

          return . modifyCurrentCanvasInfo (const (CanvasInfoBox ncinfo)) $ xstate


