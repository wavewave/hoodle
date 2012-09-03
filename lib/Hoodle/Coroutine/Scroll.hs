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
import           Control.Lens
import           Control.Monad
import           Control.Monad.State 
-- from hoodle-platform
import           Control.Monad.Trans.Crtn 
import           Data.Xournal.BBox
-- from this package
import           Hoodle.Type.Event 
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Canvas
import           Hoodle.Type.XournalState
import           Hoodle.Type.PageArrangement
import qualified Hoodle.ModelAction.Adjustment as A
import           Hoodle.Coroutine.Draw
import           Hoodle.Accessor
import           Hoodle.View.Coordinate
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
  where hscrollmoveAction = modifyCurrentCanvasInfo (selectBox fsimple fsimple)
        fsimple cinfo = 
          let BBox vm_orig _ = unViewPortBBox $ view (viewInfo.pageArrangement.viewPortBBox) cinfo
          in over (viewInfo.pageArrangement.viewPortBBox) (apply (moveBBoxULCornerTo (v,snd vm_orig))) $ cinfo


-- | 
vscrollBarMoved :: CanvasId -> Double -> MainCoroutine ()         
vscrollBarMoved cid v = 
    chkCvsIdNInvalidate cid 
    >> updateXState (return . vscrollmoveAction) 
    >> invalidate cid
  where vscrollmoveAction = modifyCurrentCanvasInfo (selectBox fsimple fsimple)
        fsimple cinfo =  
          let BBox vm_orig _ = unViewPortBBox $ view (viewInfo.pageArrangement.viewPortBBox) cinfo
          in over (viewInfo.pageArrangement.viewPortBBox) (apply (moveBBoxULCornerTo (fst vm_orig,v))) $ cinfo

-- | 
vscrollStart :: CanvasId -> MainCoroutine () 
vscrollStart cid = do 
  chkCvsIdNInvalidate cid 
  vscrollMove cid 
        

-- |                   
vscrollMove :: CanvasId -> MainCoroutine () 
vscrollMove cid = do    
    ev <- nextevent 
    xst <- get 
    geometry <- liftIO (getCanvasGeometryCvsId cid xst)
    case ev of
      VScrollBarMoved cid' v -> do 
        when (cid /= cid') $ error "something wrong in vscrollMove"
        updateXState $ return.modifyCurrentCanvasInfo 
                         (selectBox (scrollmovecanvas v) (scrollmovecanvasCont geometry v))
        invalidateWithBuf cid 
        vscrollMove cid 
      VScrollBarEnd cid' v -> do 
        when (cid /= cid') $ error "something wrong in vscrollMove"        
        updateXState $ return.modifyCurrentCanvasInfo 
                         (selectBox (scrollmovecanvas v) (scrollmovecanvasCont geometry v)) 
        invalidate cid' 
        return ()
      VScrollBarStart cid' _v -> vscrollStart cid' 
      _ -> return ()       
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








