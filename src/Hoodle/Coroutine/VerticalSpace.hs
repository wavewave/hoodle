-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.VerticalSpace
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.VerticalSpace where

import Control.Category
-- import Data.Label
import qualified Data.IntMap as IM
import Control.Lens
import Control.Monad.State 
import qualified Control.Monad.State as St
import Data.Time.Clock
import           Graphics.Rendering.Cairo 
import           Graphics.UI.Gtk hiding (get,set) 
-- 
import Data.Hoodle.Generic
import Data.Hoodle.BBox
import           Data.Hoodle.Simple

import Graphics.Hoodle.Render
import Graphics.Hoodle.Render.Type.Item 
import Graphics.Hoodle.Render.Util.HitTest
-- 
import           Hoodle.Accessor
import           Hoodle.Coroutine.Draw
import           Hoodle.Coroutine.Page
import           Hoodle.Coroutine.Scroll
import           Hoodle.Coroutine.Select
import Hoodle.Coroutine.Commit
import Hoodle.Coroutine.Pen 
import           Hoodle.Device
import           Hoodle.ModelAction.Page 
import           Hoodle.ModelAction.Select
import Hoodle.Type.Event
import Hoodle.Type.Coroutine
import Hoodle.Type.Canvas
import Hoodle.Type.HoodleState
import Hoodle.Type.PageArrangement
import Hoodle.Type.Predefined 
import Hoodle.Device
import Hoodle.View.Coordinate
import Hoodle.View.Draw

import Hoodle.Accessor
import Hoodle.ModelAction.Page
import Hoodle.ModelAction.Eraser
import Hoodle.ModelAction.Layer
import Hoodle.Util
--
import Prelude hiding ((.), id)

-- |
verticalSpaceStart :: CanvasId 
               -> PointerCoord 
               -> MainCoroutine () 
verticalSpaceStart cid = commonPenStart verticalSpaceAction cid  
  where verticalSpaceAction _cinfo pnum geometry (x,y) = do 
          hdl <- liftM getHoodle get 
          itms <- rItmsInCurrLyr
          (tempsurface,Dim w h) <- liftIO $ canvasImageSurface Nothing geometry hdl 
          
          tempsurface2 <- liftIO $ createImageSurface FormatARGB32 (floor w) (floor h)
          ctime <- liftIO getCurrentTime 
          verticalSpaceProcess cid pnum geometry itms (x,y) (tempsurface,tempsurface2) ctime 

          liftIO $ surfaceFinish tempsurface 
          liftIO $ surfaceFinish tempsurface2
          
-- |

verticalSpaceProcess :: CanvasId
                     -> PageNum 
                     -> CanvasGeometry
                     -> [RItem] 
                     -> (Double,Double)
                     -> (Surface,Surface)
                     -> UTCTime
                     -> MainCoroutine () 
verticalSpaceProcess cid pnum geometry itms (x0,y0) (sfc1,sfc2) otime = do 
    r <- nextevent 
    xst <- get
    boxAction (f r xst) . getCanvasInfo cid $ xst 
  where 
    CvsCoord (x0_cvs,y0_cvs) = 
      (desktop2Canvas geometry . page2Desktop geometry) (pnum,PageCoord (x0,y0))
    
    f :: (ViewMode a) => MyEvent -> HoodleState -> CanvasInfo a -> MainCoroutine ()
    f r xstate cvsInfo = penMoveAndUpOnly r pnum geometry defact 
                           (moveact xstate cvsInfo) upact
                           
    defact = verticalSpaceProcess cid pnum geometry itms (x0,y0) (sfc1,sfc2) otime
    upact _ = invalidateAll 
    moveact xstate cvsInfo (pcoord,(x,y)) = do 
          ctime <- liftIO getCurrentTime 
          let dtime = diffUTCTime ctime otime 
              willUpdate = dtime > dtime_bound 
          if willUpdate  
            then do 
              let CvsCoord (x_cvs,y_cvs) = 
                    (desktop2Canvas geometry . page2Desktop geometry) (pnum,PageCoord (x,y))
              liftIO $ renderWith sfc2 $ do 
                save 
                setSourceSurface sfc1 0 (y_cvs-y0_cvs)
                setOperator OperatorSource
                paint
                restore 
              let canvas = view drawArea cvsInfo 
              win <- liftIO $ widgetGetDrawWindow canvas
              liftIO $ renderWithDrawable win $ do 
                setSourceSurface sfc2 0 0 
                setOperator OperatorSource 
                paint 
              liftIO $ print (x,y,x0,y0)
              verticalSpaceProcess cid pnum geometry itms (x0,y0) (sfc1,sfc2) ctime
            else 
              verticalSpaceProcess cid pnum geometry itms (x0,y0) (sfc1,sfc2) otime

        
    
{-    

    f r xstate cvsInfo = penMoveAndUpOnly r pnum geometry defact 
                                 (moveact xstate cvsInfo) upact
    defact = verticalSpaceProcess cid pnum geometry itms (x0,y0)
    upact _ = invalidateAll
    moveact xstate cvsInfo (_pcoord,(x,y)) = do 
      let line = ((x0,y0),(x,y))
          hittestbbox = hltHittedByLineRough line itms
          (hittestitem,hitState) = 
            St.runState (hltItmsHittedByLineFrmSelected_StateT line hittestbbox) False
      if hitState 
        then do 
          page <- getCurrentPageCvsId cid 
          let currhdl     = unView . view hoodleModeState $ xstate 
              dim         = view gdimension page
              pgnum       = view currentPageNum cvsInfo
              currlayer = getCurrentLayer page
          let (newitms,maybebbox1) = St.runState (eraseHitted hittestitem) Nothing
              maybebbox = fmap (flip inflate 2.0) maybebbox1
          newlayerbbox <- liftIO . updateLayerBuf dim maybebbox 
                          . set gitems newitms $ currlayer 
          let newpagebbox = adjustCurrentLayer newlayerbbox page 
              newhdlbbox = over gpages (IM.adjust (const newpagebbox) pgnum) currhdl
              newhdlmodst = ViewAppendState newhdlbbox
          commit . set hoodleModeState newhdlmodst 
            =<< (liftIO (updatePageAll newhdlmodst xstate))
          invalidateInBBox Nothing Efficient cid 
          nitms <- rItmsInCurrLyr
          verticalSpaceProcess cid pnum geometry nitms (x,y)
        else verticalSpaceProcess cid pnum geometry itms (x,y) 
  -}          
