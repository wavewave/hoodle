{-# LANGUAGE ScopedTypeVariables, GADTs #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Default 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Widget.Dispatch where

import Control.Applicative ((<|>))
import Control.Lens (view,set,over)
import Control.Monad.State 
import Control.Monad.Trans.Maybe
import Data.Time 
import Graphics.Rendering.Cairo 
-- 
import Data.Hoodle.BBox
import Data.Hoodle.Simple 
import Graphics.Hoodle.Render.Util.HitTest
-- 
import Hoodle.Device 
import Hoodle.Type.Canvas
import Hoodle.Type.Coroutine
import Hoodle.Type.HoodleState
import Hoodle.Type.Widget
import Hoodle.Type.PageArrangement
import Hoodle.View.Coordinate 
import Hoodle.View.Draw 
import Hoodle.Widget.Layer
import Hoodle.Widget.PanZoom


widgetCheckPen :: CanvasId 
               -> PointerCoord 
               -> MainCoroutine ()    -- ^ default action 
               -> MainCoroutine ()
widgetCheckPen cid pcoord defact = boxAction chk =<< liftM (getCanvasInfo cid) get
  where 
    chk :: (ViewMode a) => CanvasInfo a -> MainCoroutine () 
    chk cinfo = do 
      let cvs = view drawArea cinfo
          pnum = (PageNum . view currentPageNum) cinfo 
          arr = view (viewInfo.pageArrangement) cinfo
      geometry <- liftIO $ makeCanvasGeometry pnum arr cvs 
      let triplet = (cid,cinfo,geometry)
      m <- runMaybeT $ 
             (lift . startPanZoomWidget triplet <=< MaybeT . return . checkPointerInPanZoom triplet) pcoord
             <|> 
             (lift . startLayerWidget triplet <=< MaybeT . return . checkPointerInLayer triplet) pcoord
      case m of        
        Nothing -> defact 
        Just _ -> return ()
