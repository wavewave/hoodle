{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Scroll 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Scroll where

import           Control.Category
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Control.Monad.State 
import           Control.Monad.Trans.Either
import           Graphics.UI.Gtk hiding (get,set)
-- from hoodle-platform
import           Control.Monad.Trans.Crtn
import           Data.Hoodle.BBox
-- from this package
import           Hoodle.Type.Event 
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Canvas
import           Hoodle.Type.HoodleState
import           Hoodle.Type.PageArrangement
import qualified Hoodle.ModelAction.Adjustment as A
import           Hoodle.Coroutine.Draw
import           Hoodle.Accessor
import           Hoodle.View.Coordinate
import           Hoodle.View.Draw
--
import           Prelude hiding ((.), id)

-- | 
adjustScrollbarWithGeometryCvsId :: CanvasId -> MainCoroutine ()
adjustScrollbarWithGeometryCvsId cid = do
  xstate <- get
  let cinfobox = getCanvasInfo cid xstate
  geometry <- liftIO (getCanvasGeometryCvsId cid xstate)
  let (hadj,vadj) = unboxGet adjustments cinfobox 
      connidh = unboxGet horizAdjConnId cinfobox 
      connidv = unboxGet vertAdjConnId cinfobox
  liftIO $ A.adjustScrollbarWithGeometry geometry ((hadj,connidh),(vadj,connidv))  


-- | 
adjustScrollbarWithGeometryCurrent :: MainCoroutine ()
adjustScrollbarWithGeometryCurrent = do
  xstate <- get
  geometry <- liftIO . getGeometry4CurrCvs $ xstate
  let cinfobox = view currentCanvasInfo xstate
  let (hadj,vadj) = unboxGet adjustments cinfobox 
      connidh = unboxGet horizAdjConnId cinfobox 
      connidv = unboxGet vertAdjConnId cinfobox
  liftIO $ A.adjustScrollbarWithGeometry geometry ((hadj,connidh),(vadj,connidv))  

-- | 
hscrollBarMoved :: CanvasId -> Double -> MainCoroutine ()         
hscrollBarMoved cid v = 
    changeCurrentCanvasId cid 
    >> updateXState (return . hscrollmoveAction) 
    >> invalidate cid 
  where hscrollmoveAction = over currentCanvasInfo (selectBox fsimple fsimple)
        fsimple cinfo = 
          let BBox vm_orig _ = unViewPortBBox $ view (viewInfo.pageArrangement.viewPortBBox) cinfo
          in over (viewInfo.pageArrangement.viewPortBBox) (apply (moveBBoxULCornerTo (v,snd vm_orig))) $ cinfo


-- | 
vscrollBarMoved :: CanvasId -> Double -> MainCoroutine ()         
vscrollBarMoved cid v = 
    chkCvsIdNInvalidate cid 
    >> updateXState (return . vscrollmoveAction) 
    >> invalidate cid
       -- invalidateInBBox Nothing Efficient cid 
  where vscrollmoveAction = over currentCanvasInfo (selectBox fsimple fsimple)
        fsimple cinfo =  
          let BBox vm_orig _ = unViewPortBBox $ view (viewInfo.pageArrangement.viewPortBBox) cinfo
          in over (viewInfo.pageArrangement.viewPortBBox) (apply (moveBBoxULCornerTo (fst vm_orig,v))) $ cinfo

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
    geometry <- liftIO (getCanvasGeometryCvsId cid xst)
    case ev of
      VScrollBarMoved cid' v -> do 
        when (cid /= cid') $ 
          (lift . hoistEither . Left . Other) "something wrong in vscrollMove"
        smoothScroll cid geometry v0 v 
        vscrollMove cid v 
      VScrollBarEnd cid' v -> do 
        when (cid /= cid') $ 
          (lift . hoistEither . Left . Other) "something wrong in vscrollMove" 
        smoothScroll cid geometry v0 v
        invalidateAll 
        return ()
      VScrollBarStart cid' v -> vscrollStart cid' v 
      _ -> return ()       



-- | 
smoothScroll :: CanvasId -> CanvasGeometry -> Double -> Double -> MainCoroutine () 
smoothScroll cid geometry v0 v = do 
    xst <- get 
    let b = view doesSmoothScroll xst 
    let diff = (v - v0) 
        lst'  | (diff < 20 && diff > -20) = [v]
              | (diff < 5 &&diff > -5) = []
              | otherwise = let m :: Int = floor (minimum [20, (abs diff) / 10.0])
                                delta = diff / fromIntegral m            
                            in [v0 + fromIntegral n*delta | n <- [1..m] ]
        lst | b  = lst'                     
            | otherwise = [v] 
    forM_ lst $ \v' -> do 
      updateXState $ return . over currentCanvasInfo 
                     (selectBox (scrollmovecanvas v) (scrollmovecanvasCont geometry v'))
      invalidateInBBox Nothing Efficient cid 
      -- liftIO $ threadDelay (floor (100 * abs diff))
  where scrollmovecanvas v cvsInfo = 
          let BBox vm_orig _ = unViewPortBBox $ view (viewInfo.pageArrangement.viewPortBBox) cvsInfo
          in over (viewInfo.pageArrangement.viewPortBBox) 
                  (apply (moveBBoxULCornerTo (fst vm_orig,v))) cvsInfo 
             
        scrollmovecanvasCont geometry v cvsInfo = 
          let BBox vm_orig _ = unViewPortBBox $ view (viewInfo.pageArrangement.viewPortBBox) cvsInfo
              cpn = PageNum . view currentPageNum $ cvsInfo 
              ncpn = maybe cpn fst $ desktop2Page geometry (DeskCoord (0,v))
          in  over currentPageNum (const (unPageNum ncpn)) 
              . over (viewInfo.pageArrangement.viewPortBBox) 
                       (apply (moveBBoxULCornerTo (fst vm_orig,v))) $ cvsInfo 

