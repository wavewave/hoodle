{-# LANGUAGE GADTs #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Mode 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Mode where

import           Control.Applicative
import           Control.Category
-- import Data.Label
import           Control.Lens
import           Control.Monad.Trans
import qualified Data.IntMap as M
import           Graphics.UI.Gtk (adjustmentGetValue)
--
import           Data.Hoodle.BBox
import           Data.Hoodle.Generic
import           Graphics.Hoodle.Render.BBoxMapPDF
-- from this package
import           Hoodle.Accessor
import           Hoodle.Coroutine.Scroll
import           Hoodle.Coroutine.Draw
import           Hoodle.Type.Alias
import           Hoodle.Type.Canvas
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Event
import           Hoodle.Type.HoodleState
import           Hoodle.Type.PageArrangement
import           Hoodle.View.Coordinate
--
import Prelude hiding ((.),id, mapM_, mapM)

modeChange :: MyEvent -> MainCoroutine () 
modeChange command = case command of 
                       ToViewAppendMode -> updateXState select2edit >> invalidateAll 
                       ToSelectMode     -> updateXState edit2select >> invalidateAll 
                       _ -> return ()
  where select2edit xst =  
          either (noaction xst) (whenselect xst) . hoodleModeStateEither . view hoodleModeState $ xst
        edit2select xst = 
          either (whenedit xst) (noaction xst) . hoodleModeStateEither . view hoodleModeState $ xst
        noaction :: HoodleState -> a -> MainCoroutine HoodleState
        noaction xstate = const (return xstate)
        whenselect :: HoodleState -> Hoodle SelectMode -> MainCoroutine HoodleState
        whenselect xstate thdl = do 
          let pages = view g_selectAll thdl
              mselect = view g_selectSelected thdl
          npages <- maybe (return pages) 
                          (\(spgn,spage) -> do 
                             npage <- liftIO $ resetPageBuffers (gcast spage)  
                             return $ M.adjust (const npage) spgn pages )
                          mselect
          return . flip (set hoodleModeState) xstate 
            . ViewAppendState . GHoodle (view g_selectTitle thdl) $ npages 
        whenedit :: HoodleState -> Hoodle EditMode -> MainCoroutine HoodleState   
        whenedit xstate hdl = return . flip (set hoodleModeState) xstate 
                              . SelectState  
                              $ GSelect (view g_title hdl) (gpages hdl) Nothing

-- | 

viewModeChange :: MyEvent -> MainCoroutine () 
viewModeChange command = do 
    case command of 
      ToSinglePage -> updateXState cont2single >> invalidateAll 
      ToContSinglePage -> updateXState single2cont >> invalidateAll 
      _ -> return ()
    adjustScrollbarWithGeometryCurrent     
  where cont2single xst =  
          selectBoxAction (noaction xst) (whencont xst) . view currentCanvasInfo $ xst
        single2cont xst = 
          selectBoxAction (whensing xst) (noaction xst) . view currentCanvasInfo $ xst
        noaction :: HoodleState -> a -> MainCoroutine HoodleState  
        noaction xstate = const (return xstate)

        whencont xstate cinfo = do 
          geometry <- liftIO $ getGeometry4CurrCvs xstate 
          cdim <- liftIO $  return . canvasDim $ geometry 
                  --  =<< getCanvasGeometry xstate 
          page <- getCurrentPageCurr
          let zmode = view (viewInfo.zoomMode) cinfo
              canvas = view drawArea cinfo 
              cpn = PageNum . view currentPageNum $ cinfo 

              pdim = PageDimension (view g_dimension page )
              ViewPortBBox bbox = view (viewInfo.pageArrangement.viewPortBBox) cinfo       
              (x0,y0) = bbox_upperleft bbox 
              (xpos,ypos) = maybe (0,0) (unPageCoord.snd) $ desktop2Page geometry (DeskCoord (x0,y0))  
          let arr = makeSingleArrangement zmode pdim cdim (xpos,ypos) 
          let nvinfo = ViewInfo (view zoomMode (view viewInfo cinfo)) arr 
              ncinfo = CanvasInfo (view canvasId cinfo)
                                  canvas
                                  (view scrolledWindow cinfo)
                                  nvinfo 
                                  (unPageNum cpn)
                                  (view horizAdjustment cinfo)
                                  (view vertAdjustment cinfo)
                                  (view horizAdjConnId cinfo)
                                  (view vertAdjConnId cinfo)
          liftIO $ putStrLn " after "                                   
          liftIO $ printCanvasMode (getCurrentCanvasId xstate) (CanvasInfoBox ncinfo)
          return $ set currentCanvasInfo (CanvasInfoBox ncinfo) xstate

        whensing xstate cinfo = do 
          cdim <- liftIO $  return . canvasDim =<< getGeometry4CurrCvs xstate 
          let zmode = view (viewInfo.zoomMode) cinfo
              canvas = view drawArea cinfo 
              cpn = PageNum . view currentPageNum $ cinfo 
              (hadj,vadj) = view adjustments cinfo 
          (xpos,ypos) <- liftIO $ (,) <$> adjustmentGetValue hadj <*> adjustmentGetValue vadj

          let arr = makeContinuousSingleArrangement zmode cdim (getHoodle xstate) 
                                                    (cpn, PageCoord (xpos,ypos))
              -- ContinuousSingleArrangement _ (DesktopDimension (Dim w h)) _ _ = arr  
          geometry <- liftIO $ makeCanvasGeometry cpn arr canvas
          let DeskCoord (nxpos,nypos) = page2Desktop geometry (cpn,PageCoord (xpos,ypos))
          let vinfo = view viewInfo cinfo 
              nvinfo = ViewInfo (view zoomMode vinfo) arr 
              ncinfotemp = CanvasInfo (view canvasId cinfo)
                                      (view drawArea cinfo)
                                      (view scrolledWindow cinfo)
                                      nvinfo 
                                      (view currentPageNum cinfo)
                                      hadj 
                                      vadj 
                                      (view horizAdjConnId cinfo)
                                      (view vertAdjConnId cinfo)
              ncpn = maybe cpn fst $ desktop2Page geometry (DeskCoord (nxpos,nypos))
              ncinfo = over currentPageNum (const (unPageNum ncpn)) ncinfotemp

          return . modifyCurrentCanvasInfo (const (CanvasInfoBox ncinfo)) $ xstate


