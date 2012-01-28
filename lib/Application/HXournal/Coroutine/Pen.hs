-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.Coroutine.Pen 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
module Application.HXournal.Coroutine.Pen where

import Graphics.UI.Gtk hiding (get,set,disconnect)
import Application.HXournal.Device 
import Application.HXournal.Type.Event
import Application.HXournal.Type.Enum
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.PageArrangement
import Application.HXournal.Type.XournalState
import Application.HXournal.Coroutine.Draw
import Application.HXournal.Coroutine.EventConnect
import Application.HXournal.Coroutine.Commit
import Application.HXournal.Accessor
import Application.HXournal.Util
import Application.HXournal.ModelAction.Pen
import Application.HXournal.ModelAction.Page
import Application.HXournal.Draw
import Control.Monad.Trans

import Data.Xournal.Predefined
import Data.Xournal.Generic
import Control.Monad.Coroutine.SuspensionFunctors
import Data.Sequence hiding (filter)
import qualified Data.Map as M
import Data.Maybe 
import Control.Category
import Data.Label
import Prelude hiding ((.), id)
import Graphics.Xournal.Render.BBox

-- | Common Pen Work starting point 

commonPenStart :: ( CanvasInfo SinglePage -> CanvasPageGeometry -> ZoomMode 
                    -> (ConnectId DrawingArea, ConnectId DrawingArea) 
                    -> (Double,Double) -> MainCoroutine () )
               -> CanvasId -> PointerCoord 
               -> MainCoroutine ()
commonPenStart action cid pcoord =
    selectBoxAction fsingle (error "commonPenStart") . getCanvasInfo cid =<< changeCurrentCanvasId cid 
  where fsingle cvsInfo = do 
          let zmode = get (zoomMode.viewInfo) cvsInfo     
          geometry <- getCanvasGeometry cvsInfo
          let (x,y) = device2pageCoord geometry zmode pcoord 
          connidup   <- connectPenUp cvsInfo 
          connidmove <- connectPenMove cvsInfo
          action cvsInfo geometry zmode (connidup,connidmove) (x,y) 
      
-- | enter pen drawing mode

penStart :: CanvasId -> PointerCoord -> MainCoroutine () 
penStart cid = commonPenStart penAction cid 
  where penAction cinfo cpg zmode (cidmove,cidup) (x,y) = do 
          xstate <- getSt
          let currxoj = unView . get xournalstate $ xstate        
              pagenum = get currentPageNum cinfo          
              pinfo = get penInfo xstate
          pdraw <-penProcess cid cpg cidmove cidup (empty |> (x,y)) (x,y) 
          (newxoj,bbox) <- liftIO $ addPDraw pinfo currxoj pagenum pdraw
          let bbox' = inflate bbox (get (penWidth.currentTool.penInfo) xstate) 
              xstate' = set xournalstate (ViewAppendState newxoj) 
                        . updatePageAll (ViewAppendState newxoj)
                        $ xstate
          commit xstate'
          invalidateAll 

-- | main pen coordinate adding process

penProcess :: CanvasId
           -> CanvasPageGeometry
           -> ConnectId DrawingArea -> ConnectId DrawingArea 
           -> Seq (Double,Double) -> (Double,Double) 
           -> MainCoroutine (Seq (Double,Double))
penProcess cid cpg connidmove connidup pdraw (x0,y0) = do 
    r <- await 
    xst <- getSt 
    selectBoxAction (fsingle r xst) (error "penProcess") . getCanvasInfo cid $ xst
  where 
    fsingle r xstate cvsInfo = do 
      case r of 
        PenMove _cid' pcoord -> do 
          let canvas = get drawArea cvsInfo
              zmode  = get (zoomMode.viewInfo) cvsInfo
              ptype  = get (penType.penInfo) xstate
              pcolor = get (penColor.currentTool.penInfo) xstate 
              pwidth = get (penWidth.currentTool.penInfo) xstate 
              (x,y) = device2pageCoord cpg zmode pcoord 
              (pcr,pcg,pcb,pca)= fromJust (M.lookup pcolor penColorRGBAmap) 
              opacity = case ptype of 
                          HighlighterWork -> predefined_highlighter_opacity 
                          _ -> 1.0
              pcolRGBA = (pcr,pcg,pcb,pca*opacity)
          liftIO $ drawSegment canvas cpg zmode pwidth pcolRGBA (x0,y0) (x,y)
          penProcess cid cpg connidmove connidup (pdraw |> (x,y)) (x,y) 
        PenUp _cid' pcoord -> do 
          let zmode = get (zoomMode.viewInfo) cvsInfo
              (x,y) = device2pageCoord cpg zmode pcoord 
          disconnect connidmove
          disconnect connidup
          return (pdraw |> (x,y)) 
        _ -> do
          penProcess cid cpg connidmove connidup pdraw (x0,y0) 
