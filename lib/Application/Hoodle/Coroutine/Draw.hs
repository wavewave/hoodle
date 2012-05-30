{-# LANGUAGE Rank2Types #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Application.Hoodle.Coroutine.Draw 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Application.Hoodle.Coroutine.Draw where

import Application.Hoodle.Type.Coroutine
import Application.Hoodle.Type.Canvas
import Application.Hoodle.Type.XournalState
import Application.Hoodle.Type.PageArrangement
import Application.Hoodle.View.Draw
import Application.Hoodle.Accessor
import Data.Xournal.BBox
import Control.Applicative 
import Control.Monad
import Control.Monad.Trans
import qualified Data.IntMap as M
import Control.Category
import Data.Label
import Prelude hiding ((.),id)
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk hiding (get,set)
import Application.Hoodle.Type.Alias

-- |

data DrawingFunctionSet = 
  DrawingFunctionSet { singleEditDraw :: DrawingFunction SinglePage EditMode
                     , singleSelectDraw :: DrawingFunction SinglePage SelectMode
                     , contEditDraw :: DrawingFunction ContinuousSinglePage EditMode
                     , contSelectDraw :: DrawingFunction ContinuousSinglePage SelectMode 
                     }

-- | 

invalidateGeneral :: CanvasId -> Maybe BBox 
                  -> DrawingFunction SinglePage EditMode
                  -> DrawingFunction SinglePage SelectMode
                  -> DrawingFunction ContinuousSinglePage EditMode
                  -> DrawingFunction ContinuousSinglePage SelectMode
                  -> MainCoroutine () 
invalidateGeneral cid mbbox drawf drawfsel drawcont drawcontsel = do 
    xst <- getSt 
    selectBoxAction (fsingle xst) (fcont xst) . getCanvasInfo cid $ xst
  where fsingle :: HoodleState -> CanvasInfo SinglePage -> MainCoroutine () 
        fsingle xstate cvsInfo = do 
          let cpn = PageNum . get currentPageNum $ cvsInfo 
              isCurrentCvs = cid == getCurrentCanvasId xstate
              epage = getCurrentPageEitherFromXojState cvsInfo (get xournalstate xstate)
          case epage of 
            Left page -> do  
              liftIO (unSinglePageDraw drawf isCurrentCvs 
                        <$> get drawArea <*> pure (cpn,page) 
                        <*> get viewInfo <*> pure mbbox $ cvsInfo )
            Right tpage -> do 
              liftIO (unSinglePageDraw drawfsel isCurrentCvs
                        <$> get drawArea <*> pure (cpn,tpage) 
                        <*> get viewInfo <*> pure mbbox $ cvsInfo )
        fcont :: HoodleState -> CanvasInfo ContinuousSinglePage -> MainCoroutine () 
        fcont xstate cvsInfo = do 
          let xojstate = get xournalstate xstate 
              isCurrentCvs = cid == getCurrentCanvasId xstate
          case xojstate of 
            ViewAppendState xoj -> do  
              liftIO (unContPageDraw drawcont isCurrentCvs cvsInfo mbbox xoj)
            SelectState txoj -> 
              liftIO (unContPageDraw drawcontsel isCurrentCvs cvsInfo mbbox txoj)
          
-- |         

invalidateOther :: MainCoroutine () 
invalidateOther = do 
  xstate <- getSt
  let currCvsId = getCurrentCanvasId xstate
      cinfoMap  = getCanvasInfoMap xstate
      keys = M.keys cinfoMap 
  mapM_ invalidate (filter (/=currCvsId) keys)
  
-- | invalidate clear 

invalidate :: CanvasId -> MainCoroutine () 
invalidate = invalidateInBBox Nothing 

-- | 

invalidateInBBox :: Maybe BBox -- ^ desktop coord
                    -> CanvasId -> MainCoroutine ()
invalidateInBBox mbbox cid = do 
  invalidateGeneral cid mbbox
    drawPageClearly drawPageSelClearly drawContXojClearly drawContXojSelClearly

-- | 

invalidateAllInBBox :: Maybe BBox -- ^ desktop coordinate 
                       -> MainCoroutine ()
invalidateAllInBBox mbbox = do                        
  xstate <- getSt
  let cinfoMap  = getCanvasInfoMap xstate
      keys = M.keys cinfoMap 
  forM_ keys (invalidateInBBox mbbox)

-- | 

invalidateAll :: MainCoroutine () 
invalidateAll = invalidateAllInBBox Nothing

-- | Invalidate Current canvas

invalidateCurrent :: MainCoroutine () 
invalidateCurrent = invalidate . getCurrentCanvasId =<< getSt
       
-- | Drawing temporary gadgets

invalidateTemp :: CanvasId -> Surface ->  Render () -> MainCoroutine ()
invalidateTemp cid tempsurface rndr = do 
    xst <- getSt 
    selectBoxAction (fsingle xst) (fsingle xst) . getCanvasInfo cid $ xst 
  where fsingle xstate cvsInfo = do 
          let canvas = get drawArea cvsInfo
              pnum = PageNum . get currentPageNum $ cvsInfo 
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
    xst <- getSt 
    selectBoxAction (fsingle xst) (fsingle xst) . getCanvasInfo cid $ xst 
  where fsingle xstate cvsInfo = do 
          let canvas = get drawArea cvsInfo
          geometry <- liftIO $ getCanvasGeometryCvsId cid xstate
          win <- liftIO $ widgetGetDrawWindow canvas
          let xformfunc = cairoXform4PageCoordinate geometry pnum
          liftIO $ renderWithDrawable win $ do   
                     setSourceSurface tempsurface 0 0 
                     setOperator OperatorSource 
                     paint 
                     xformfunc 
                     rndr 
      
-- | Drawing using layer buffer
 
invalidateWithBuf :: CanvasId -> MainCoroutine () 
invalidateWithBuf = invalidateWithBufInBBox Nothing
  
-- | Drawing using layer buffer in BBox  

invalidateWithBufInBBox :: Maybe BBox -> CanvasId -> MainCoroutine () 
invalidateWithBufInBBox mbbox cid =  
  invalidateGeneral cid mbbox drawBuf drawSelBuf drawContXojBuf drawContXojSelClearly

-- | check current canvas id and new active canvas id and invalidate if it's changed. 

chkCvsIdNInvalidate :: CanvasId -> MainCoroutine () 
chkCvsIdNInvalidate cid = do 
  currcid <- liftM (getCurrentCanvasId) getSt 
  when (currcid /= cid) (changeCurrentCanvasId cid >> invalidateAll)
  
