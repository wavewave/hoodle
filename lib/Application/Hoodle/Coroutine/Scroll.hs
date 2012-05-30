-----------------------------------------------------------------------------
-- |
-- Module      : Application.Hoodle.Coroutine.Scroll 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Application.Hoodle.Coroutine.Scroll where

import Application.Hoodle.Type.Event 
import Application.Hoodle.Type.Coroutine
import Application.Hoodle.Type.Canvas
import Application.Hoodle.Type.XournalState
import Application.Hoodle.Type.PageArrangement
import qualified Application.Hoodle.ModelAction.Adjustment as A
import Application.Hoodle.Coroutine.Draw
import Application.Hoodle.Accessor
import Application.Hoodle.View.Coordinate
import Application.Hoodle.Util
import Control.Monad
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Category
import Data.Xournal.BBox
import Data.Label
import Control.Monad.Trans
import Prelude hiding ((.), id)

-- | 

adjustScrollbarWithGeometryCvsId :: CanvasId -> MainCoroutine ()
adjustScrollbarWithGeometryCvsId cid = do
  xstate <- getSt
  let cinfobox = getCanvasInfo cid xstate
  geometry <- liftIO (getCanvasGeometryCvsId cid xstate)
  let (hadj,vadj) = unboxGet adjustments cinfobox 
      connidh = unboxGet horizAdjConnId cinfobox 
      connidv = unboxGet vertAdjConnId cinfobox
  liftIO $ A.adjustScrollbarWithGeometry geometry ((hadj,connidh),(vadj,connidv))  


-- | 

adjustScrollbarWithGeometryCurrent :: MainCoroutine ()
adjustScrollbarWithGeometryCurrent = do
  xstate <- getSt
  geometry <- liftIO . getGeometry4CurrCvs $ xstate
  let cinfobox = get currentCanvasInfo xstate
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
          let BBox vm_orig _ = unViewPortBBox $ get (viewPortBBox.pageArrangement.viewInfo) cinfo
          in modify (viewPortBBox.pageArrangement.viewInfo) (apply (moveBBoxULCornerTo (v,snd vm_orig))) $ cinfo


-- | 
        
vscrollBarMoved :: CanvasId -> Double -> MainCoroutine ()         
vscrollBarMoved cid v = 
    -- changeCurrentCanvasId cid 
    chkCvsIdNInvalidate cid 
    >> updateXState (return . vscrollmoveAction) 
    >> invalidate cid
  where vscrollmoveAction = modifyCurrentCanvasInfo (selectBox fsimple fsimple)
        fsimple cinfo =  
          let BBox vm_orig _ = unViewPortBBox $ get (viewPortBBox.pageArrangement.viewInfo) cinfo
          in modify (viewPortBBox.pageArrangement.viewInfo) (apply (moveBBoxULCornerTo (fst vm_orig,v))) $ cinfo

-- | 

vscrollStart :: CanvasId -> MainCoroutine () 
vscrollStart cid = do 
  chkCvsIdNInvalidate cid 
  vscrollMove cid 
        

-- |                   

vscrollMove :: CanvasId -> MainCoroutine () 
vscrollMove cid = do    
    ev <- await 
    xst <- getSt 
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
      VScrollBarStart cid v -> vscrollStart cid 
      other -> return ()       
  where scrollmovecanvas v cvsInfo = 
          let BBox vm_orig _ = unViewPortBBox $ get (viewPortBBox.pageArrangement.viewInfo) cvsInfo
          in modify (viewPortBBox.pageArrangement.viewInfo) 
                    (apply (moveBBoxULCornerTo (fst vm_orig,v))) cvsInfo 
             
        scrollmovecanvasCont geometry v cvsInfo = 
          let BBox vm_orig _ = unViewPortBBox $ get (viewPortBBox.pageArrangement.viewInfo) cvsInfo
              cpn = PageNum . get currentPageNum $ cvsInfo 
              ncpn = maybe cpn fst $ desktop2Page geometry (DeskCoord (0,v))
          in  modify currentPageNum (const (unPageNum ncpn)) 
              . modify (viewPortBBox.pageArrangement.viewInfo) 
                       (apply (moveBBoxULCornerTo (fst vm_orig,v))) $ cvsInfo 








