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
-----------------------------------------------------------------------------

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
import Application.HXournal.ModelAction.Pen
import Application.HXournal.ModelAction.Page
import Application.HXournal.View.Coordinate
import Application.HXournal.View.Draw
import Application.HXournal.Util
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Coroutine.SuspensionFunctors
import Data.Xournal.Predefined
import Data.Sequence hiding (filter)
import qualified Data.Map as M
import Data.Maybe 
import Control.Category
import Data.Label
import Prelude hiding ((.), id)

-- | Common Pen Work starting point 

commonPenStart :: ( CanvasInfo SinglePage -> PageNum -> CanvasGeometry  
                    -> (ConnectId DrawingArea, ConnectId DrawingArea) 
                    -> (Double,Double) -> MainCoroutine () )
               -> CanvasId -> PointerCoord 
               -> MainCoroutine ()
commonPenStart action cid pcoord =
    selectBoxAction fsingle (error "commonPenStart") . getCanvasInfo cid =<< changeCurrentCanvasId cid 
  where fsingle :: CanvasInfo SinglePage -> MainCoroutine ()
        fsingle cvsInfo = do 
          let page = getPage cvsInfo
              cpn = PageNum . get currentPageNum $ cvsInfo
              arr = get (pageArrangement.viewInfo) cvsInfo              
              canvas = get drawArea cvsInfo
          geometry <- liftIO $ makeCanvasGeometry (cpn,page) arr canvas
          let pagecoord = desktop2Page geometry . device2Desktop geometry $ pcoord 
          maybeFlip pagecoord (return ()) 
            $ \(pgn,PageCoord (x,y)) ->  when (cpn == pgn) $ do 
                 connidup   <- connectPenUp cvsInfo 
                 connidmove <- connectPenMove cvsInfo
                 action cvsInfo pgn geometry (connidup,connidmove) (x,y) 

      
-- | enter pen drawing mode

penStart :: CanvasId -> PointerCoord -> MainCoroutine () 
penStart cid = commonPenStart penAction cid 
  where penAction cinfo pnum@(PageNum cpn) geometry (cidmove,cidup) (x,y) = do 
          xstate <- getSt
          let currxoj = unView . get xournalstate $ xstate        
              pinfo = get penInfo xstate
          pdraw <-penProcess cid pnum geometry cidmove cidup (empty |> (x,y)) (x,y) 
          (newxoj,_bbox) <- liftIO $ addPDraw pinfo currxoj pnum pdraw
          let xstate' = set xournalstate (ViewAppendState newxoj) 
                        . updatePageAll (ViewAppendState newxoj)
                        $ xstate
          commit xstate'
          invalidateAll 

-- | main pen coordinate adding process

-- | now being changed

penProcess :: CanvasId -> PageNum 
           -> CanvasGeometry
           -> ConnectId DrawingArea -> ConnectId DrawingArea 
           -> Seq (Double,Double) -> (Double,Double) 
           -> MainCoroutine (Seq (Double,Double))
penProcess cid pnum geometry connidmove connidup pdraw (x0,y0) = do 
    r <- await 
    xst <- getSt 
    selectBoxAction (fsingle r xst) (error "penProcess") . getCanvasInfo cid $ xst
  where 
    fsingle r xstate cvsInfo = 
      penMoveAndUpOnly r pnum geometry 
        (penProcess cid pnum geometry connidmove connidup pdraw (x0,y0))
        (\(x,y) -> do 
           let canvas = get drawArea cvsInfo
               ptype  = get (penType.penInfo) xstate
               pcolor = get (penColor.currentTool.penInfo) xstate 
               pwidth = get (penWidth.currentTool.penInfo) xstate 
               (pcr,pcg,pcb,pca)= fromJust (M.lookup pcolor penColorRGBAmap) 
               opacity = case ptype of 
                  HighlighterWork -> predefined_highlighter_opacity 
                  _ -> 1.0
               pcolRGBA = (pcr,pcg,pcb,pca*opacity) 
           liftIO $ drawCurvebit canvas geometry pwidth pcolRGBA pnum (x0,y0) (x,y)
           penProcess cid pnum geometry connidmove connidup (pdraw |> (x,y)) (x,y) )
        (\_ -> disconnect connidmove >> disconnect connidup >> return pdraw )

      {-
      case r of 
        PenMove _cid' pcoord -> do 
          let pagecoord = desktop2Page geometry . device2Desktop geometry $ pcoord 
          
          maybeFlip pagecoord 
            (penProcess cid pnum geometry connidmove connidup pdraw (x0,y0)) 
            $ \(pgn,PageCoord (x,y)) -> 
                if (pnum /= pgn)  
                then (penProcess cid pnum geometry connidmove connidup pdraw (x0,y0))
                else do 
                  let canvas = get drawArea cvsInfo
                      ptype  = get (penType.penInfo) xstate
                      pcolor = get (penColor.currentTool.penInfo) xstate 
                      pwidth = get (penWidth.currentTool.penInfo) xstate 
                      (pcr,pcg,pcb,pca)= fromJust (M.lookup pcolor penColorRGBAmap) 
                      opacity = case ptype of 
                                  HighlighterWork -> predefined_highlighter_opacity 
                                  _ -> 1.0
                      pcolRGBA = (pcr,pcg,pcb,pca*opacity)
                  liftIO $ drawCurvebit canvas geometry pwidth pcolRGBA pnum (x0,y0) (x,y)

                  penProcess cid pnum geometry connidmove connidup (pdraw |> (x,y)) (x,y) 
        PenUp _cid' pcoord -> do 
          disconnect connidmove
          disconnect connidup
          return pdraw 
        _ -> do
          penProcess cid pnum geometry connidmove connidup pdraw (x0,y0) 


          -- liftIO $ drawSegment canvas cpg zmode pwidth pcolRGBA (x0,y0) (x,y)
-}
      
skipIfNotInSamePage :: Monad m => 
                       PageNum -> CanvasGeometry -> PointerCoord 
                       -> m a -> ((Double,Double) -> m a) -> m a
skipIfNotInSamePage pgn geometry pcoord skipaction realaction = do 
    let pagecoord = desktop2Page geometry . device2Desktop geometry $ pcoord 
   
    maybeFlip pagecoord skipaction 
      $ \(cpn, PageCoord (x,y)) -> if pgn == cpn then realaction (x,y) else skipaction
                                                                 
        

penMoveAndUpOnly :: Monad m => MyEvent 
                    -> PageNum 
                    -> CanvasGeometry 
                    -> m a 
                    -> ((Double,Double) -> m a) 
                    -> (PointerCoord -> m a) 
                    -> m a
penMoveAndUpOnly r pgn geometry defact moveaction upaction = 
  case r of 
    PenMove _ pcoord -> skipIfNotInSamePage pgn geometry pcoord defact moveaction  
    PenUp _ pcoord -> upaction pcoord  
    _ -> defact 
  






