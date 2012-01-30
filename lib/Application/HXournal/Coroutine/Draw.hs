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

-- |
invalidateGeneral :: CanvasId -> Maybe BBox 
                  -> PageDrawingFunction
                  -> PageDrawFSel 
                  -> MainCoroutine () 
invalidateGeneral cid mbbox drawf drawfsel = 
    selectBoxAction fsingle (error "invalidateSelSingle") . getCanvasInfo cid =<< getSt 
  where fsingle cvsInfo = do 
          let cpn = PageNum . get currentPageNum $ cvsInfo 
          case get currentPage cvsInfo of 
            Left page ->  liftIO (drawf <$> get drawArea <*> pure (cpn,page) <*> get viewInfo <*> pure mbbox 
                                  $ cvsInfo )
            Right tpage -> liftIO (drawfsel <$> get drawArea <*> pure tpage <*> get viewInfo 
                                            <*> pure mbbox $ cvsInfo )



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


invalidateAll :: MainCoroutine () 
invalidateAll = do
  xstate <- getSt
  let cinfoMap  = get canvasInfoMap xstate
      keys = M.keys cinfoMap 
  forM_ keys invalidate 

invalidateOther :: MainCoroutine () 
invalidateOther = do 
  xstate <- getSt
  let currCvsId = get currentCanvasId xstate
      cinfoMap  = get canvasInfoMap xstate
      keys = M.keys cinfoMap 
  mapM_ invalidate (filter (/=currCvsId) keys)
  

-- | invalidate clear 

invalidate :: CanvasId -> MainCoroutine () 
invalidate cid = invalidateGeneral cid Nothing drawPageClearly drawPageSelClearly


-- | Drawing objects only in BBox, obsolete 
-- 
-- invalidateInBBox :: CanvasId -> BBox -> MainCoroutine () 
-- invalidateInBBox cid bbox = invalidateSelSingle cid (Just bbox) drawPageInBBox drawSelectionInBBox

-- | Drawing BBox
--
invalidateDrawBBox :: CanvasId -> BBox -> MainCoroutine () 
invalidateDrawBBox cid bbox = invalidateSelSingle cid (Just bbox) drawBBox drawBBoxSel



-- | Drawing using layer buffer
 
invalidateWithBuf :: CanvasId -> MainCoroutine () 
invalidateWithBuf = invalidateWithBufInBBox Nothing
  

-- | Drawing using layer buffer in BBox  

invalidateWithBufInBBox :: Maybe BBox -> CanvasId -> MainCoroutine () 
invalidateWithBufInBBox mbbox cid = invalidateSelSingle cid mbbox drawBuf drawSelectionInBBox
                             

-- | Invalidate Current canvas

invalidateCurrent :: MainCoroutine () 
invalidateCurrent = invalidate . get currentCanvasId =<< getSt
       
-- | Drawing temporary gadgets

invalidateTemp :: CanvasId -> Surface -> Render () -> MainCoroutine ()
invalidateTemp cid tempsurface rndr = do 
    selectBoxAction fsingle (error "invalidateTemp") . getCanvasInfo cid =<< getSt 
  where fsingle cvsInfo = do 
          let page = either id gcast $ get currentPage cvsInfo 
              canvas = get drawArea cvsInfo
              vinfo = get viewInfo cvsInfo      
          let zmode  = get zoomMode vinfo
              BBox origin _ = unViewPortBBox $ get (viewPortBBox.pageArrangement) vinfo
          geometry <- liftIO $ getCanvasPageGeometry canvas page origin
          win <- liftIO $ widgetGetDrawWindow canvas
          let xformfunc = transformForPageCoord geometry zmode
          liftIO $ renderWithDrawable win $ do   
                     setSourceSurface tempsurface 0 0 
                     setOperator OperatorSource 
                     paint 
                     xformfunc 
                     rndr 
      
