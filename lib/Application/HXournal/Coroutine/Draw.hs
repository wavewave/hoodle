{-# LANGUAGE Rank2Types #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.Coroutine.Draw 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Application.HXournal.Coroutine.Draw where

import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.XournalState
import Application.HXournal.Type.PageArrangement
import Application.HXournal.View.Draw
import Application.HXournal.Accessor
import Data.Xournal.BBox
import Control.Applicative 
import Control.Monad
import Control.Monad.Trans
import qualified Data.IntMap as M
import Control.Category
import Data.Label
import Prelude hiding ((.),id)
import Data.Xournal.Generic
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk hiding (get,set)
import Application.HXournal.View.Coordinate
import Application.HXournal.Type.Alias
import Application.HXournal.ModelAction.Page

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
    selectBoxAction fsingle fcont . getCanvasInfo cid $ xst
  where fsingle :: CanvasInfo SinglePage -> MainCoroutine () 
        fsingle cvsInfo = do 
          let cpn = PageNum . get currentPageNum $ cvsInfo 
          case get currentPage cvsInfo of 
            Left page -> do  
              liftIO (unSinglePageDraw drawf 
                        <$> get drawArea <*> pure (cpn,page) 
                        <*> get viewInfo <*> pure mbbox $ cvsInfo )
            Right tpage -> do 
              liftIO (unSinglePageDraw drawfsel 
                        <$> get drawArea <*> pure (cpn,tpage) 
                        <*> get viewInfo <*> pure mbbox $ cvsInfo )
        fcont :: CanvasInfo ContinuousSinglePage -> MainCoroutine () 
        fcont cvsInfo = do 
          xojstate <- liftM (get xournalstate) getSt 
          case xojstate of 
            ViewAppendState xoj -> do  
              liftIO (unContPageDraw drawcont cvsInfo mbbox xoj)
            SelectState txoj -> 
              liftIO (unContPageDraw drawcontsel cvsInfo mbbox txoj)
          
        

invalidateOther :: MainCoroutine () 
invalidateOther = do 
  xstate <- getSt
  let currCvsId = get currentCanvasId xstate
      cinfoMap  = get canvasInfoMap xstate
      keys = M.keys cinfoMap 
  mapM_ invalidate (filter (/=currCvsId) keys)
  

-- | invalidate clear 

invalidate :: CanvasId -> MainCoroutine () 
invalidate = invalidateInBBox Nothing 

{-  invalidateGeneral cid Nothing 
    drawPageClearly drawPageSelClearly drawContXojClearly drawContXojSelClearly-} 

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
  let cinfoMap  = get canvasInfoMap xstate
      keys = M.keys cinfoMap 
  forM_ keys (invalidateInBBox mbbox)

-- | 

invalidateAll :: MainCoroutine () 
invalidateAll = invalidateAllInBBox Nothing



-- | Invalidate Current canvas

invalidateCurrent :: MainCoroutine () 
invalidateCurrent = invalidate . get currentCanvasId =<< getSt
       
-- | Drawing temporary gadgets

invalidateTemp :: CanvasId -> Surface -> Render () -> MainCoroutine ()
invalidateTemp cid tempsurface rndr = do 
    xst <- getSt 
    selectBoxAction (fsingle xst) (fsingle xst) . getCanvasInfo cid $ xst 
  where fsingle xstate cvsInfo = do 
          let page = either id gcast $ get currentPage cvsInfo 
              canvas = get drawArea cvsInfo
              vinfo = get viewInfo cvsInfo      
              pnum = PageNum . get currentPageNum $ cvsInfo 
          geometry <- liftIO $ getCanvasGeometry xstate
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



{-
-- | obsolete 

invalidateSelSingle :: CanvasId -> Maybe BBox 
                       -> PageDrawF
                       -> PageDrawFSel 
                       -> MainCoroutine () 
invalidateSelSingle cid mbbox drawf drawfsel = 
    selectBoxAction fsingle (error "invalidateSelSingle") . getCanvasInfo cid =<< getSt 
  where fsingle cvsInfo = do 
          case get currentPage cvsInfo of 
            Left page ->  liftIO (drawf <$> get drawArea <*> pure page <*> get viewInfo <*> pure mbbox 
                                  $ cvsInfo )
            Right tpage -> liftIO (drawfsel <$> get drawArea <*> pure tpage <*> get viewInfo 
                                            <*> pure mbbox $ cvsInfo )




invalidateGenSingle :: CanvasId -> Maybe BBox -> PageDrawF
                    -> MainCoroutine () 
invalidateGenSingle cid mbbox drawf = 
    selectBoxAction fsingle (error "invalidateGenSingle") . getCanvasInfo cid =<< getSt 
  where fsingle cvsInfo = do 
          let page = case get currentPage cvsInfo of
                       Right _ -> error "no invalidateGenSingle implementation yet"
                       Left pg -> pg
          liftIO (drawf <$> get drawArea 
                        <*> pure page 
                        <*> get viewInfo 
                        <*> pure mbbox
                        $ cvsInfo )

-}

{-
-- | Drawing objects only in BBox, obsolete 
-- 
-- invalidateInBBox :: CanvasId -> BBox -> MainCoroutine () 
-- invalidateInBBox cid bbox = invalidateSelSingle cid (Just bbox) drawPageInBBox drawSelectionInBBox

-- | Drawing BBox
--
invalidateDrawBBox :: CanvasId -> BBox -> MainCoroutine () 
invalidateDrawBBox cid bbox = invalidateSelSingle cid (Just bbox) drawBBox drawBBoxSel



-}                             
