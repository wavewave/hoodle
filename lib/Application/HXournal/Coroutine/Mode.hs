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
import Application.HXournal.Coroutine.Scroll
import Application.HXournal.Coroutine.Draw
import Control.Applicative
import Control.Monad.Trans
import Control.Category
import Data.Label
import qualified Data.IntMap as M
import Data.Xournal.BBox
import Data.Xournal.Generic
import Graphics.Xournal.Render.BBoxMapPDF
import Graphics.UI.Gtk (adjustmentGetValue)
import Prelude hiding ((.),id, mapM_, mapM)

modeChange :: MyEvent -> MainCoroutine () 
modeChange command = case command of 
                       ToViewAppendMode -> updateXState select2edit >> invalidateAll 
                       ToSelectMode     -> updateXState edit2select >> invalidateAll 
                       _ -> return ()
  where select2edit xst =  
          either (noaction xst) (whenselect xst) . xojstateEither . get xournalstate $ xst
        edit2select xst = 
          either (whenedit xst) (noaction xst) . xojstateEither . get xournalstate $ xst
        noaction :: HXournalState -> a -> MainCoroutine HXournalState
        noaction xstate = const (return xstate)
        whenselect :: HXournalState -> Xournal SelectMode -> MainCoroutine HXournalState
        whenselect xstate txoj = do 
          let pages = get g_selectAll txoj
              mselect = get g_selectSelected txoj
          npages <- maybe (return pages) 
                          (\(spgn,spage) -> do 
                             npage <- liftIO $ resetPageBuffers (gcast spage)  
                             return $ M.adjust (const npage) spgn pages )
                          mselect
          return . flip (set xournalstate) xstate 
            . ViewAppendState . GXournal (get g_selectTitle txoj) $ npages 
        whenedit :: HXournalState -> Xournal EditMode -> MainCoroutine HXournalState   
        whenedit xstate xoj = return . flip (set xournalstate) xstate 
                              . SelectState  
                              $ GSelect (get g_title xoj) (gpages xoj) Nothing

--   M.adjust (const ( 
--   =<< liftIO (mapM resetPageBuffers (get g_selectAll txoj)) 

-- | 

viewModeChange :: MyEvent -> MainCoroutine () 
viewModeChange command = do 
    case command of 
      ToSinglePage -> updateXState cont2single >> invalidateAll 
      ToContSinglePage -> updateXState single2cont >> invalidateAll 
      _ -> return ()
    adjustScrollbarWithGeometryCurrent     
  where cont2single xst =  
          selectBoxAction (noaction xst) (whencont xst) . get currentCanvasInfo $ xst
        single2cont xst = 
          selectBoxAction (whensing xst) (noaction xst) . get currentCanvasInfo $ xst
        noaction :: HXournalState -> a -> MainCoroutine HXournalState  
        noaction xstate = const (return xstate)

        whencont xstate cinfo = do 
          geometry <- liftIO $ getCanvasGeometry xstate 
          cdim <- liftIO $  return . canvasDim =<< getCanvasGeometry xstate 
          page <- getCurrentPageCurr
          let zmode = get (zoomMode.viewInfo) cinfo
              canvas = get drawArea cinfo 
              cpn = PageNum . get currentPageNum $ cinfo 

              pdim = PageDimension (get g_dimension page )
              ViewPortBBox bbox = get (viewPortBBox.pageArrangement.viewInfo) cinfo       
              (x0,y0) = bbox_upperleft bbox 
              (xpos,ypos) = maybe (0,0) (unPageCoord.snd) $ desktop2Page geometry (DeskCoord (x0,y0))  
          let arr = makeSingleArrangement zmode pdim cdim (xpos,ypos) 
          let nvinfo = ViewInfo (get zoomMode (get viewInfo cinfo)) arr 
              ncinfo = CanvasInfo (get canvasId cinfo)
                                  canvas
                                  (get scrolledWindow cinfo)
                                  nvinfo 
                                  (unPageNum cpn)
                                  (get horizAdjustment cinfo)
                                  (get vertAdjustment cinfo)
                                  (get horizAdjConnId cinfo)
                                  (get vertAdjConnId cinfo)
          liftIO $ putStrLn " after "                                   
          liftIO $ printCanvasMode (getCurrentCanvasId xstate) (CanvasInfoBox ncinfo)
          return $ set currentCanvasInfo (CanvasInfoBox ncinfo) xstate

        whensing xstate cinfo = do 
          cdim <- liftIO $  return . canvasDim =<< getCanvasGeometry xstate 
          -- page <- getCurrentPageCurr 
          let zmode = get (zoomMode.viewInfo) cinfo
              canvas = get drawArea cinfo 
              cpn = PageNum . get currentPageNum $ cinfo 
              (hadj,vadj) = get adjustments cinfo 
          (xpos,ypos) <- liftIO $ (,) <$> adjustmentGetValue hadj <*> adjustmentGetValue vadj

          let arr = makeContinuousSingleArrangement zmode cdim (getXournal xstate) 
                                                    (cpn, PageCoord (xpos,ypos))
              -- ContinuousSingleArrangement _ (DesktopDimension (Dim w h)) _ _ = arr  
          geometry <- liftIO $ makeCanvasGeometry cpn arr canvas
          let DeskCoord (nxpos,nypos) = page2Desktop geometry (cpn,PageCoord (xpos,ypos))
          let vinfo = get viewInfo cinfo 
              nvinfo = ViewInfo (get zoomMode vinfo) arr 
              ncinfotemp = CanvasInfo (get canvasId cinfo)
                                      (get drawArea cinfo)
                                      (get scrolledWindow cinfo)
                                      nvinfo 
                                      (get currentPageNum cinfo)
                                      hadj 
                                      vadj 
                                      (get horizAdjConnId cinfo)
                                      (get vertAdjConnId cinfo)
              ncpn = maybe cpn fst $ desktop2Page geometry (DeskCoord (nxpos,nypos))
              ncinfo = modify currentPageNum (const (unPageNum ncpn)) ncinfotemp

          return . modifyCurrentCanvasInfo (const (CanvasInfoBox ncinfo)) $ xstate


