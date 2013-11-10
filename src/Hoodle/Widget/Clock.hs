-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Widget.Clock
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Clock widget drawing and action
-- 
-----------------------------------------------------------------------------

module Hoodle.Widget.Clock where

import           Control.Lens (view,set,over)
import Control.Monad.State 
import Control.Monad.Trans
import           Data.Functor.Identity (Identity(..))
import           Data.List (delete)
import           Data.Sequence
import           Data.Time
import           Graphics.Rendering.Cairo
--
import           Data.Hoodle.BBox
import           Data.Hoodle.Simple 
import           Graphics.Hoodle.Render.Util.HitTest
--
import           Hoodle.Coroutine.Draw
import           Hoodle.Coroutine.Layer
import           Hoodle.Coroutine.Pen
import           Hoodle.Device
import           Hoodle.ModelAction.Select
import           Hoodle.Type.Canvas
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Enum
import           Hoodle.Type.Event
import           Hoodle.Type.HoodleState 
import           Hoodle.Type.PageArrangement
import           Hoodle.Type.Widget
import           Hoodle.View.Coordinate
import           Hoodle.View.Draw
--
import Debug.Trace

-- | 
data CWAction = Move (CanvasCoordinate,CanvasCoordinate)
                deriving (Show)
                         

checkPointerInClock :: (CanvasId,CanvasInfo a,CanvasGeometry) 
                    -> PointerCoord 
                    -> Maybe CWAction 
checkPointerInClock (cid,cinfo,geometry) pcoord 
  | b =  
    let oxy@(CvsCoord (x,y)) = (desktop2Canvas geometry . device2Desktop geometry) pcoord
        owxy@(CvsCoord (x0,y0)) = view (canvasWidgets.clockWidgetConfig.clockWidgetPosition) cinfo
        obbox = BBox (x0-50,y0-50) (x0+50,y0+50) 
        r | isPointInBBox obbox (x,y) = Just (Move (oxy,owxy))
          | otherwise                 = Nothing
    in r
  | otherwise = Nothing 
  where b = view (canvasWidgets.widgetConfig.doesUseClockWidget) cinfo 

-- |
startClockWidget :: (CanvasId,CanvasInfo a,CanvasGeometry)
                 -> CWAction 
                 -> MainCoroutine () 
startClockWidget (cid,cinfo,geometry) (Move (oxy,owxy)) = do 
    xst <- get 
    let hdl = getHoodle xst
    (srcsfc,Dim wsfc hsfc) <- liftIO (canvasImageSurface Nothing geometry hdl)
    -- need to draw other widgets here                             
    let otherwidgets = delete ClockWidget allWidgets 
    liftIO $ renderWith srcsfc (drawWidgets otherwidgets hdl cinfo Nothing) 
    -- end : need to draw other widgets here ^^^
    tgtsfc <- liftIO $ createImageSurface FormatARGB32 (floor wsfc) (floor hsfc)
    ctime <- liftIO getCurrentTime 
    let CvsCoord (x0,y0) = owxy 
        CvsCoord (x,y) = oxy 
    manipulateCW cid geometry (srcsfc,tgtsfc) owxy oxy ctime 
    liftIO $ surfaceFinish srcsfc 
    liftIO $ surfaceFinish tgtsfc


-- | main event loop for clock widget
manipulateCW :: CanvasId 
             -> CanvasGeometry 
             -> (Surface,Surface) 
             -> CanvasCoordinate 
             -> CanvasCoordinate 
             -> UTCTime 
             -> MainCoroutine () 
manipulateCW cid geometry (srcsfc,tgtsfc) 
             owxy@(CvsCoord (xw,yw)) oxy@(CvsCoord (x0,y0)) otime = do 
    r <- nextevent
    case r of 
      PenMove _ pcoord -> do 
        processWithDefTimeInterval
          (manipulateCW cid geometry (srcsfc,tgtsfc) owxy oxy) 
          (\ctime -> moveClockWidget cid geometry (srcsfc,tgtsfc) owxy oxy pcoord 
                     >> manipulateCW cid geometry (srcsfc,tgtsfc) owxy oxy ctime)
          otime 
      PenUp _ pcoord -> invalidate cid 
      _ -> return ()

moveClockWidget :: CanvasId 
                   -> CanvasGeometry 
                   -> (Surface,Surface) 
                   -> CanvasCoordinate 
                   -> CanvasCoordinate 
                   -> PointerCoord
                   -> MainCoroutine ()
moveClockWidget cid geometry (srcsfc,tgtsfc) (CvsCoord (xw,yw)) (CvsCoord (x0,y0)) pcoord = do 
    let CvsCoord (x,y) = (desktop2Canvas geometry . device2Desktop geometry) pcoord 
    xst <- get 
    let CanvasDimension (Dim cw ch) = canvasDim geometry 
        cinfobox = getCanvasInfo cid xst 
        nposx | xw+x-x0 < -50 = -50 
              | xw+x-x0 > cw-50 = cw-50 
              | otherwise = xw+x-x0
        nposy | yw+y-y0 < -50 = -50 
              | yw+y-y0 > ch-50 = ch-50 
              | otherwise = yw+y-y0                             
        nwpos = CvsCoord (nposx,nposy) 
        changeact :: CanvasInfo a -> CanvasInfo a 
        changeact cinfo =  
          set (canvasWidgets.clockWidgetConfig.clockWidgetPosition) nwpos $ cinfo
        ncinfobox = (runIdentity . forBoth unboxBiXform (return . changeact)) cinfobox
    put (setCanvasInfo (cid,ncinfobox) xst)
    -- 
    xst2 <- get 
    let cinfobox = getCanvasInfo cid xst2 
        cfg = view (unboxLens (canvasWidgets.clockWidgetConfig)) cinfobox
    liftIO $ forBoth' unboxBiAct (\cinfo-> virtualDoubleBufferDraw srcsfc tgtsfc (return ()) 
                                    (renderClockWidget Nothing cfg) 
                                  >> doubleBufferFlush tgtsfc cinfo) cinfobox



-- | 
toggleClock :: CanvasId -> MainCoroutine () 
toggleClock cid = do 
  modify (\xst -> 
            let ncinfobox = (over (unboxLens (canvasWidgets.widgetConfig.doesUseClockWidget)) not 
                             . getCanvasInfo cid ) xst 
            in setCanvasInfo (cid,ncinfobox) xst)
  invalidateInBBox Nothing Efficient cid

