-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Eraser 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Eraser where

import Control.Category
-- import Data.Label
import qualified Data.IntMap as IM
import Control.Lens
import Control.Monad.State 
import Control.Monad.Trans.Crtn
import qualified Control.Monad.State as St
import Graphics.UI.Gtk hiding (get,set,disconnect)
-- 
import Data.Hoodle.Generic
import Data.Hoodle.BBox
import Graphics.Hoodle.Render.Util.HitTest
import Graphics.Hoodle.Render
-- 
import Hoodle.Type.Event
import Hoodle.Type.Coroutine
import Hoodle.Type.Canvas
import Hoodle.Type.HoodleState
import Hoodle.Type.PageArrangement
import Hoodle.Device
-- import Hoodle.View.Draw
import Hoodle.View.Coordinate
import Hoodle.Coroutine.EventConnect
import Hoodle.Coroutine.Draw
import Hoodle.Coroutine.Commit
import Hoodle.Accessor
import Hoodle.ModelAction.Page
import Hoodle.ModelAction.Eraser
import Hoodle.ModelAction.Layer
import Hoodle.Coroutine.Pen 
--
import Prelude hiding ((.), id)

-- |

eraserStart :: CanvasId 
               -> PointerCoord 
               -> MainCoroutine () 
eraserStart cid = commonPenStart eraserAction cid  
  where eraserAction _cinfo pnum geometry (cidup,cidmove) (x,y) = do 
          strs <- getAllStrokeBBoxInCurrentLayer
          eraserProcess cid pnum geometry cidup cidmove strs (x,y)

-- |

eraserProcess :: CanvasId
              -> PageNum 
              -> CanvasGeometry
              -> ConnectId DrawingArea -> ConnectId DrawingArea 
              -> [StrokeBBox] 
              -> (Double,Double)
              -> MainCoroutine () 
eraserProcess cid pnum geometry connidmove connidup strs (x0,y0) = do 
    r <- nextevent 
    xst <- get
    boxAction (f r xst) . getCanvasInfo cid $ xst 
  where 
    f :: (ViewMode a) => MyEvent -> HoodleState -> CanvasInfo a -> MainCoroutine ()
    f r xstate cvsInfo = penMoveAndUpOnly r pnum geometry defact 
                                 (moveact xstate cvsInfo) upact
    defact = eraserProcess cid pnum geometry connidup connidmove strs (x0,y0)
    upact _ = disconnect connidmove >> disconnect connidup >> invalidateAll
    moveact xstate cvsInfo (_pcoord,(x,y)) = do 
      let line = ((x0,y0),(x,y))
          hittestbbox = hltStrksHittedByLineRough line strs   
          (hitteststroke,hitState) = 
            St.runState (hitTestStrokes line hittestbbox) False
      if hitState 
        then do 
          page <- getCurrentPageCvsId cid 
          let currhdl     = unView . view hoodleModeState $ xstate 
              pgnum       = view currentPageNum cvsInfo
              (mcurrlayer, currpage) = getCurrentLayerOrSet page
              currlayer = maybe (error "eraserProcess") id mcurrlayer
          let (newstrokes,maybebbox1) = St.runState (eraseHitted hitteststroke) Nothing
              maybebbox = fmap (flip inflate 2.0) maybebbox1
          newlayerbbox <- liftIO . updateLayerBuf maybebbox 
                          . set gstrokes newstrokes $ currlayer 
          let newpagebbox = adjustCurrentLayer newlayerbbox currpage 
              newhdlbbox = over gpages (IM.adjust (const newpagebbox) pgnum) currhdl
              newhdlmodst = ViewAppendState newhdlbbox
          commit . set hoodleModeState newhdlmodst 
            =<< (liftIO (updatePageAll newhdlmodst xstate))
          invalidateWithBuf cid 
          newstrs <- getAllStrokeBBoxInCurrentLayer
          eraserProcess cid pnum geometry connidup connidmove newstrs (x,y)
        else eraserProcess cid pnum geometry connidmove connidup strs (x,y) 
            
