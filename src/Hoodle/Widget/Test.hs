-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Widget.Test
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Widget.Test where

-- from other packages
import           Control.Category
import           Control.Lens (view,set,over)
import           Control.Monad.Identity 
import           Control.Monad.State 
import           Data.IORef
import           Graphics.UI.Gtk hiding (get,set)
import qualified Graphics.UI.Gtk as Gtk (get)
-- from hoodle-platform 
import           Data.Hoodle.BBox
import           Data.Hoodle.Generic
import           Data.Hoodle.Zipper
import           Graphics.Hoodle.Render.Type
import           Graphics.Hoodle.Render.Util.HitTest
-- 
import           Hoodle.Coroutine.Draw
import           Hoodle.Device
import           Hoodle.Type.Alias
import           Hoodle.Type.Canvas
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Event
import           Hoodle.Type.HoodleState 
import           Hoodle.Type.PageArrangement 
import           Hoodle.View.Coordinate
import           Hoodle.View.Draw
-- 
import Prelude hiding ((.),id)



widgetCheckPen :: CanvasId -> PointerCoord 
               -> MainCoroutine () 
               -> MainCoroutine ()
widgetCheckPen cid pcoord act = do 
    xst <- get
    let cinfobox = getCanvasInfo cid xst 
    boxAction f cinfobox 
  where 
    f cinfo = do 
      let cvs = view drawArea cinfo
          pnum = (PageNum . view currentPageNum) cinfo 
          arr = view (viewInfo.pageArrangement) cinfo
      geometry <- liftIO $ makeCanvasGeometry pnum arr cvs 
      let oxy@(CvsCoord (x,y)) = (desktop2Canvas geometry . device2Desktop geometry) pcoord
      let owxy@(CvsCoord (x0,y0)) = view (canvasWidgets.testWidgetPosition) cinfo
          bbox = BBox (x0,y0) (x0+100,y0+100) 
      if (isPointInBBox bbox (x,y)) 
         then startWidgetAction cid geometry owxy oxy
         else act 
widgetCheckPen cid pcoord act = act  


startWidgetAction :: CanvasId 
                     -> CanvasGeometry 
                     -> CanvasCoordinate -- ^ original widget position
                     -> CanvasCoordinate -- ^ where pen pressed 
                     -> MainCoroutine ()
startWidgetAction cid geometry owxy@(CvsCoord (xw,yw)) oxy@(CvsCoord (x0,y0)) = do
  r <- nextevent
  case r of 
    PenMove _ pcoord -> do 
      let CvsCoord (x,y) = (desktop2Canvas geometry . device2Desktop geometry) pcoord 
      xst <- get 
      let cinfobox = getCanvasInfo cid xst 
          changeact :: (ViewMode a) => CanvasInfo a -> CanvasInfo a 
          changeact cinfo = 
            let nwpos = CvsCoord (xw+x-x0,yw+y-y0)
            in  set (canvasWidgets.testWidgetPosition) nwpos $ cinfo
          ncinfobox = selectBox changeact changeact  cinfobox
      put (setCanvasInfo (cid,ncinfobox) xst)
      invalidateInBBox Nothing Efficient cid 
      startWidgetAction cid geometry owxy oxy 
    PenUp _ pcoord -> return () 
    


