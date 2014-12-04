{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}


-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Scroll 
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Scroll where

import           Control.Lens (view,over,_1)
import           Control.Monad
import           Control.Monad.State 
import           Control.Monad.Trans.Either
import qualified Graphics.UI.Gtk as Gtk
-- from hoodle-platform
import           Control.Monad.Trans.Crtn
import           Data.Functor.Identity (Identity(..))
import           Data.Hoodle.BBox
-- from this package
import           Hoodle.Coroutine.Draw
import           Hoodle.GUI.Reflect
import           Hoodle.Type.Enum
import           Hoodle.Type.Event 
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Canvas
import           Hoodle.Type.HoodleState
import           Hoodle.Type.PageArrangement
import qualified Hoodle.ModelAction.Adjustment as A
import           Hoodle.Accessor
import           Hoodle.View.Coordinate
--

-- |
updateCanvasInfo :: (forall a. CanvasInfo a -> CanvasInfo a) -> CanvasId -> MainCoroutine ()
updateCanvasInfo f cid = pureUpdateUhdl $ \uhdl -> 
                           let cinfobox = getCanvasInfo cid uhdl
                               ncinfobox = (runIdentity . forBoth unboxBiXform (return . f)) cinfobox
                           in setCanvasInfo (cid,ncinfobox) uhdl

-- |
updateCanvasInfo2 :: (CanvasInfo SinglePage -> CanvasInfo SinglePage) 
                  -> (CanvasInfo ContinuousPage -> CanvasInfo ContinuousPage)
                  -> CanvasId
                  -> MainCoroutine ()
updateCanvasInfo2 fs fc cid = 
    pureUpdateUhdl $ \uhdl ->
      let cinfobox = getCanvasInfo cid uhdl
          ncinfobox = (runIdentity . unboxBiXform (return . fs) (return . fc)) cinfobox
      in setCanvasInfo (cid,ncinfobox) uhdl

-- | 
moveViewPortBy :: MainCoroutine ()->CanvasId-> ((Double,Double)->(Double,Double))
                  -> MainCoroutine () 
moveViewPortBy rndr cid f = 
    updateCanvasInfo moveact cid >> adjustScrollbarWithGeometryCvsId cid >> rndr 
  where     
    --updater xst = let uhdl = view (unitHoodles.currentUnit) xst
    --                  cinfobox = getCanvasInfo cid uhdl
    --                  ncinfobox = (runIdentity . forBoth unboxBiXform (return . moveact)) cinfobox       
    --              in set unitHoodles (putTheUnit (setCanvasInfo (cid,ncinfobox) uhdl)) xst
    moveact :: CanvasInfo a -> CanvasInfo a 
    moveact cinfo = 
      let BBox (x0,y0) _ = 
            (unViewPortBBox . view (viewInfo.pageArrangement.viewPortBBox)) cinfo
          DesktopDimension ddim = 
            view (viewInfo.pageArrangement.desktopDimension) cinfo
      in over (viewInfo.pageArrangement.viewPortBBox) 
           (xformViewPortFitInSize ddim (moveBBoxULCornerTo (f (x0,y0)))) 
           cinfo


-- | 
adjustScrollbarWithGeometryCvsId :: CanvasId -> MainCoroutine ()
adjustScrollbarWithGeometryCvsId cid = do
  xstate <- get
  let uhdl = view (unitHoodles.currentUnit) xstate
      cinfobox = getCanvasInfo cid uhdl
  geometry <- liftIO (getCanvasGeometryCvsId cid uhdl)
  let (hadj,vadj) = view (unboxLens adjustments) cinfobox 
      connidh = view (unboxLens horizAdjConnId) cinfobox 
      connidv = view (unboxLens vertAdjConnId) cinfobox
  doIOaction_ $ Gtk.postGUIAsync $ A.adjustScrollbarWithGeometry geometry ((hadj,connidh),(vadj,connidv))  


-- | 
adjustScrollbarWithGeometryCurrent :: MainCoroutine ()
adjustScrollbarWithGeometryCurrent = adjustScrollbarWithGeometryCvsId . view (currentCanvas._1) 
                                     . view (unitHoodles.currentUnit) =<< get 

-- | 
hscrollBarMoved :: CanvasId -> Double -> MainCoroutine ()         
hscrollBarMoved cid v = 
  changeCurrentCanvasId cid
  >> moveViewPortBy (invalidate cid) cid (\(_,y)->(v,y))

-- | 
vscrollBarMoved :: CanvasId -> Double -> MainCoroutine ()         
vscrollBarMoved cid v = chkCvsIdNInvalidate cid 
                        >> moveViewPortBy (invalidate cid) cid (\(x,_)->(x,v))
  
-- | 
vscrollStart :: CanvasId -> Double -> MainCoroutine () 
vscrollStart cid v = do 
  chkCvsIdNInvalidate cid 
  vscrollMove cid v
        
-- |                   
vscrollMove :: CanvasId -> Double -> MainCoroutine () 
vscrollMove cid v0 = do    
    ev <- nextevent 
    xst <- get 
    let uhdl = view (unitHoodles.currentUnit) xst
    geometry <- liftIO (getCanvasGeometryCvsId cid uhdl)
    case ev of
      VScrollBarMoved cid' v -> do 
        when (cid /= cid') $ 
          (lift . hoistEither . Left . Other) "something wrong in vscrollMove"
        smoothScroll cid geometry v0 v 
        vscrollMove cid v 
      VScrollBarEnd cid' v -> do 
        when (cid /= cid') $ 
          (lift . hoistEither . Left . Other) "something wrong in vscrollMove" 
        moveViewPortBy (invalidate cid) cid (\(x,_)->(x,v))          
        invalidate cid 
        return ()
      VScrollBarStart cid' v -> vscrollStart cid' v 
      _ -> return ()       

-- | 
smoothScroll :: CanvasId -> CanvasGeometry -> Double -> Double -> MainCoroutine () 
smoothScroll cid geometry _v0 v = do 
    -- xst <- get 
    let lst = [v]
    forM_ lst $ \v' -> do 
      -- updateXState $ return . updater 
      updateCanvasInfo2 (scrollmovecanvas v) (scrollmovecanvasCont geometry v') cid
      invalidateInBBox Nothing Efficient cid 
  where 
    scrollmovecanvas vv cvsInfo = 
      let BBox vm_orig _ = unViewPortBBox $ view (viewInfo.pageArrangement.viewPortBBox) cvsInfo
      in over (viewInfo.pageArrangement.viewPortBBox) 
              (apply (moveBBoxULCornerTo (fst vm_orig,vv))) cvsInfo 
    -- 
    scrollmovecanvasCont geom vv cvsInfo = 
      let BBox vm_orig _ = unViewPortBBox $ view (viewInfo.pageArrangement.viewPortBBox) cvsInfo
          cpn = PageNum . view currentPageNum $ cvsInfo 
          ncpn = maybe cpn fst $ desktop2Page geom (DeskCoord (0,vv))
      in  over currentPageNum (const (unPageNum ncpn)) 
          . over (viewInfo.pageArrangement.viewPortBBox) 
                   (apply (moveBBoxULCornerTo (fst vm_orig,vv))) $ cvsInfo 
