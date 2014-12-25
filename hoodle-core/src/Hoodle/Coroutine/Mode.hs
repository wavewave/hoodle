{-# LANGUAGE GADTs #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Mode 
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Mode where

import           Control.Applicative
import           Control.Lens (view,set,(.~))
import           Control.Monad.State 
import qualified Data.IntMap as M
import qualified Graphics.UI.Gtk as Gtk (adjustmentGetValue)
-- from hoodle-platform
import           Data.Hoodle.BBox
import           Data.Hoodle.Generic
import           Data.Hoodle.Select
import           Graphics.Hoodle.Render
import           Graphics.Hoodle.Render.Type 
-- from this package
import           Hoodle.Accessor
import           Hoodle.Coroutine.Draw
import           Hoodle.Coroutine.Scroll
import           Hoodle.GUI.Reflect
import           Hoodle.Type.Alias
import           Hoodle.Type.Canvas
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Enum
import           Hoodle.Type.Event
import           Hoodle.Type.HoodleState
import           Hoodle.Type.PageArrangement
import           Hoodle.View.Coordinate
--
import Prelude hiding (mapM_, mapM)

modeChange :: UserEvent -> MainCoroutine () 
modeChange command = do 
    case command of 
      ToViewAppendMode -> updateXState select2edit >> invalidateAll 
      ToSelectMode     -> updateXState edit2select >> invalidateAllInBBox Nothing Efficient
      _ -> return ()
    reflectPenModeUI
    reflectPenColorUI
    reflectPenWidthUI
  where select2edit xst =  
          either (noaction xst) (whenselect xst) . hoodleModeStateEither 
          . view (unitHoodles.currentUnit.hoodleModeState) $ xst
        edit2select xst = 
          either (whenedit xst) (noaction xst) . hoodleModeStateEither 
          . view (unitHoodles.currentUnit.hoodleModeState) $ xst
        noaction :: HoodleState -> a -> MainCoroutine HoodleState
        noaction xst = const (return xst)
        whenselect :: HoodleState -> Hoodle SelectMode -> MainCoroutine HoodleState
        whenselect xst thdl = do 
          let pages = view gselAll thdl
              mselect = view gselSelected thdl
              cache = view renderCache xst
              cid = getCurrentCanvasId (view (unitHoodles.currentUnit) xst)
          npages <- maybe (return pages) 
                          (\(spgn,spage) -> do 
                             npage <- (liftIO.updatePageBuf cache cid 1.0 . hPage2RPage) spage  
                             return $ M.adjust (const npage) spgn pages )
                          mselect
          let nthdl = set gselAll npages . set gselSelected Nothing $ thdl  
          return $ (unitHoodles.currentUnit.hoodleModeState .~ ViewAppendState (gSelect2GHoodle nthdl)) xst
        whenedit :: HoodleState -> Hoodle EditMode -> MainCoroutine HoodleState   
        whenedit xst hdl =
          return $ (unitHoodles.currentUnit.hoodleModeState .~ SelectState  (gHoodle2GSelect hdl)) xst


-- | 
viewModeChange :: UserEvent -> MainCoroutine () 
viewModeChange command = do 
    case command of 
      ToSinglePage -> updateUhdl cont2single >> invalidateAll 
      ToContSinglePage -> updateUhdl single2cont >> invalidateAll 
      _ -> return ()
    adjustScrollbarWithGeometryCurrent     
  where cont2single :: UnitHoodle -> MainCoroutine UnitHoodle
        cont2single uhdl = unboxBiAct (const (return uhdl)) (cont2SingPage uhdl) . view currentCanvasInfo $ uhdl
        single2cont :: UnitHoodle -> MainCoroutine UnitHoodle
        single2cont uhdl = unboxBiAct (sing2ContPage uhdl) (const (return uhdl)) . view currentCanvasInfo $ uhdl

cont2SingPage :: UnitHoodle -> CanvasInfo a -> MainCoroutine UnitHoodle
cont2SingPage uhdl cinfo = do
    geometry <- liftIO $ getGeometry4CurrCvs uhdl
    cdim <- liftIO $  return . canvasDim $ geometry 
    page <- getCurrentPageCurr
    let zmode = view (viewInfo.zoomMode) cinfo
        canvas = view drawArea cinfo 
        cpn = PageNum . view currentPageNum $ cinfo 

        pdim = PageDimension (view gdimension page)
        ViewPortBBox bbox = view (viewInfo.pageArrangement.viewPortBBox) cinfo       
        (x0,y0) = bbox_upperleft bbox 
        (xpos,ypos) = maybe (0,0) (unPageCoord.snd) $ desktop2Page geometry (DeskCoord (x0,y0))  
    let arr = makeSingleArrangement zmode pdim cdim (xpos,ypos) 
    let nvinfo = ViewInfo (view zoomMode (view viewInfo cinfo)) arr 
        ncinfo = CanvasInfo (view canvasId cinfo)
                            canvas
                            (view mDrawSurface cinfo)
                            (view scrolledWindow cinfo)
                            nvinfo 
                            (unPageNum cpn)
                            (view horizAdjustment cinfo)
                            (view vertAdjustment cinfo)
                            (view horizAdjConnId cinfo)
                            (view vertAdjConnId cinfo)
                            (view canvasWidgets cinfo)
                            (view notifiedItem cinfo)
    return $ (currentCanvasInfo .~ CanvasSinglePage ncinfo) uhdl


sing2ContPage :: UnitHoodle -> CanvasInfo a -> MainCoroutine UnitHoodle
sing2ContPage uhdl cinfo = do
    cdim <- liftIO $  return . canvasDim =<< getGeometry4CurrCvs uhdl
    let zmode = view (viewInfo.zoomMode) cinfo
        canvas = view drawArea cinfo 
        cpn = PageNum . view currentPageNum $ cinfo 
        (hadj,vadj) = view adjustments cinfo 
    (xpos,ypos) <- liftIO $ (,) <$> Gtk.adjustmentGetValue hadj <*> Gtk.adjustmentGetValue vadj
    let arr = makeContinuousArrangement zmode cdim (getHoodle uhdl) 
                                              (cpn, PageCoord (xpos,ypos))
    geometry <- liftIO $ makeCanvasGeometry cpn arr canvas
    let DeskCoord (nxpos,nypos) = page2Desktop geometry (cpn,PageCoord (xpos,ypos))
    let vinfo = view viewInfo cinfo 
        nvinfo = ViewInfo (view zoomMode vinfo) arr 
        ncinfotemp = CanvasInfo (view canvasId cinfo)
                                (view drawArea cinfo)
                                (view mDrawSurface cinfo)
                                (view scrolledWindow cinfo)
                                nvinfo 
                                (view currentPageNum cinfo)
                                hadj 
                                vadj 
                                (view horizAdjConnId cinfo)
                                (view vertAdjConnId cinfo)
                                (view canvasWidgets cinfo) 
                                (view notifiedItem cinfo)
        ncpn = maybe cpn fst $ desktop2Page geometry (DeskCoord (nxpos,nypos))
        ncinfo = (currentPageNum .~ unPageNum ncpn) ncinfotemp
    return $ (currentCanvasInfo .~ CanvasContPage ncinfo) uhdl



