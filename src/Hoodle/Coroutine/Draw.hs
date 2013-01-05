{-# LANGUAGE Rank2Types #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Draw 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Draw where

-- from other packages
import           Control.Applicative 
import           Control.Category
import qualified Data.IntMap as M
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.State
-- import Data.Label
import           Graphics.Rendering.Cairo
import           Graphics.UI.Gtk hiding (get,set)
-- from hoodle-platform
import           Data.Hoodle.BBox
-- from this package
import           Hoodle.Accessor
import           Hoodle.Type.Alias
import           Hoodle.Type.Canvas
import           Hoodle.Type.Coroutine
import           Hoodle.Type.PageArrangement
import           Hoodle.Type.HoodleState
import           Hoodle.View.Draw
-- 
import Prelude hiding ((.),id)

-- |
data DrawingFunctionSet = 
  DrawingFunctionSet { singleEditDraw :: DrawingFunction SinglePage EditMode
                     , singleSelectDraw :: DrawingFunction SinglePage SelectMode
                     , contEditDraw :: DrawingFunction ContinuousPage EditMode
                     , contSelectDraw :: DrawingFunction ContinuousPage SelectMode 
                     }

-- | 
invalidateGeneral :: CanvasId -> Maybe BBox -> DrawFlag 
                  -> DrawingFunction SinglePage EditMode
                  -> DrawingFunction SinglePage SelectMode
                  -> DrawingFunction ContinuousPage EditMode
                  -> DrawingFunction ContinuousPage SelectMode
                  -> MainCoroutine () 
invalidateGeneral cid mbbox flag drawf drawfsel drawcont drawcontsel = do 
    xst <- get 
    selectBoxAction (fsingle xst) (fcont xst) . getCanvasInfo cid $ xst
  where fsingle :: HoodleState -> CanvasInfo SinglePage -> MainCoroutine () 
        fsingle xstate cvsInfo = do 
          let cpn = PageNum . view currentPageNum $ cvsInfo 
              isCurrentCvs = cid == getCurrentCanvasId xstate
              epage = getCurrentPageEitherFromHoodleModeState cvsInfo (view hoodleModeState xstate)
              cvs = view drawArea cvsInfo
              msfc = view mDrawSurface cvsInfo 
          case epage of 
            Left page -> do  
              liftIO (unSinglePageDraw drawf isCurrentCvs (cvs,msfc) (cpn,page)
                      <$> view viewInfo <*> pure mbbox <*> pure flag $ cvsInfo )
              return ()
            Right tpage -> do 
              liftIO (unSinglePageDraw drawfsel isCurrentCvs (cvs,msfc) (cpn,tpage)
                      <$> view viewInfo <*> pure mbbox <*> pure flag $ cvsInfo )
              return ()
        fcont :: HoodleState -> CanvasInfo ContinuousPage -> MainCoroutine () 
        fcont xstate cvsInfo = do 
          let hdlmodst = view hoodleModeState xstate 
              isCurrentCvs = cid == getCurrentCanvasId xstate
          case hdlmodst of 
            ViewAppendState hdl -> do  
              hdl' <- liftIO (unContPageDraw drawcont isCurrentCvs cvsInfo mbbox hdl flag)
              put (set hoodleModeState (ViewAppendState hdl') xstate)
            SelectState thdl -> do 
              thdl' <- liftIO (unContPageDraw drawcontsel isCurrentCvs cvsInfo mbbox thdl flag)
              put (set hoodleModeState (SelectState thdl') xstate) 
          
-- |         

invalidateOther :: MainCoroutine () 
invalidateOther = do 
  xstate <- get
  let currCvsId = getCurrentCanvasId xstate
      cinfoMap  = getCanvasInfoMap xstate
      keys = M.keys cinfoMap 
  mapM_ invalidate (filter (/=currCvsId) keys)
  
-- | invalidate clear 

invalidate :: CanvasId -> MainCoroutine () 
invalidate = invalidateInBBox Nothing Clear  

-- | 

invalidateInBBox :: Maybe BBox -- ^ desktop coord
                    -> DrawFlag 
                    -> CanvasId -> MainCoroutine ()
invalidateInBBox mbbox flag cid = do 
  invalidateGeneral cid mbbox flag 
    drawSinglePage drawSinglePageSel drawContHoodle drawContHoodleSel

-- | 

invalidateAllInBBox :: Maybe BBox -- ^ desktop coordinate 
                       -> DrawFlag
                       -> MainCoroutine ()
invalidateAllInBBox mbbox flag = do                        
  xstate <- get
  let cinfoMap  = getCanvasInfoMap xstate
      keys = M.keys cinfoMap 
  forM_ keys (invalidateInBBox mbbox flag)

-- | 

invalidateAll :: MainCoroutine () 
invalidateAll = invalidateAllInBBox Nothing Clear 
 
-- | Invalidate Current canvas

invalidateCurrent :: MainCoroutine () 
invalidateCurrent = invalidate . getCurrentCanvasId =<< get
       
-- | Drawing temporary gadgets

invalidateTemp :: CanvasId -> Surface ->  Render () -> MainCoroutine ()
invalidateTemp cid tempsurface rndr = do 
    xst <- get 
    selectBoxAction (fsingle xst) (fsingle xst) . getCanvasInfo cid $ xst 
  where fsingle xstate cvsInfo = do 
          let canvas = view drawArea cvsInfo
              pnum = PageNum . view currentPageNum $ cvsInfo 
          geometry <- liftIO $ getCanvasGeometryCvsId cid xstate
          win <- liftIO $ widgetGetDrawWindow canvas
          let xformfunc = cairoXform4PageCoordinate geometry pnum
          liftIO $ renderWithDrawable win $ do   
                     setSourceSurface tempsurface 0 0 
                     setOperator OperatorSource 
                     paint 
                     xformfunc 
                     rndr 
      
-- | Drawing temporary gadgets with coordinate based on base page

invalidateTempBasePage :: CanvasId -> Surface -> PageNum -> Render () 
                          -> MainCoroutine ()
invalidateTempBasePage cid tempsurface pnum rndr = do 
    xst <- get 
    selectBoxAction (fsingle xst) (fsingle xst) . getCanvasInfo cid $ xst 
  where fsingle xstate cvsInfo = do 
          let canvas = view drawArea cvsInfo
          geometry <- liftIO $ getCanvasGeometryCvsId cid xstate
          win <- liftIO $ widgetGetDrawWindow canvas
          let xformfunc = cairoXform4PageCoordinate geometry pnum
          liftIO $ renderWithDrawable win $ do   
                     setSourceSurface tempsurface 0 0 
                     setOperator OperatorSource 
                     paint 
                     xformfunc 
                     rndr 
{-      
-- | Drawing using layer buffer
 invalidateWithBuf :: CanvasId -> MainCoroutine () 
invalidateWithBuf = invalidateWithBufInBBox Nothing
  
-- | Drawing using layer buffer in BBox  

invalidateWithBufInBBox :: Maybe BBox -> CanvasId -> MainCoroutine () 
invalidateWithBufInBBox mbbox cid =  
  invalidateGeneral cid mbbox drawBuf drawSelBuf drawContHoodleBuf drawContHoodleSelClearly
-}

-- | check current canvas id and new active canvas id and invalidate if it's changed. 

chkCvsIdNInvalidate :: CanvasId -> MainCoroutine () 
chkCvsIdNInvalidate cid = do 
  currcid <- liftM (getCurrentCanvasId) get 
  when (currcid /= cid) (changeCurrentCanvasId cid >> invalidateAll)
  
