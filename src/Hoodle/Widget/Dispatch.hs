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
import Control.Monad.State hiding (forM_)
import Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8 as B
import Data.Foldable (forM_)
import Data.Time 
import Graphics.Rendering.Cairo 
-- 
import Data.Hoodle.BBox
import Data.Hoodle.Simple 
import Graphics.Hoodle.Render.Util.HitTest
import Graphics.Hoodle.Render.Type.Item
-- 
import Hoodle.Device 
import Hoodle.ModelAction.ContextMenu
import Hoodle.Type.Canvas
import Hoodle.Type.Coroutine
import Hoodle.Type.HoodleState
import Hoodle.Type.Widget
import Hoodle.Type.PageArrangement
import Hoodle.Util
import Hoodle.View.Coordinate 
import Hoodle.View.Draw 
import Hoodle.Widget.Clock
import Hoodle.Widget.Layer
import Hoodle.Widget.PanZoom

widgetCheckPen :: CanvasId 
               -> PointerCoord 
               -> MainCoroutine ()    -- ^ default action 
               -> MainCoroutine ()
widgetCheckPen cid pcoord defact = get >>= \xst -> unboxAct (chk xst) (getCanvasInfo cid xst) 
  where 
    chk :: HoodleState -> CanvasInfo a -> MainCoroutine () 
    chk xstate cinfo = do 
      let cvs = view drawArea cinfo
          pnum = (PageNum . view currentPageNum) cinfo 
          arr = view (viewInfo.pageArrangement) cinfo
      geometry <- liftIO $ makeCanvasGeometry pnum arr cvs 
      let triplet = (cid,cinfo,geometry)
      m <- runMaybeT $ 
             (lift . startPanZoomWidget PenMode triplet <=< MaybeT . return . checkPointerInPanZoom triplet) pcoord
             <|> 
             (lift . startLayerWidget triplet <=< MaybeT . return . checkPointerInLayer triplet) pcoord
             <|>
             (lift . startClockWidget triplet <=< MaybeT . return . checkPointerInClock triplet) pcoord
             <|> 
             (do guard (view (settings.doesFollowLinks) xstate)   
                 (pnum,bbox,ritem) <- (MaybeT . return . view notifiedItem) cinfo
                 (pnum',PageCoord (x,y)) <- (MaybeT . return . desktop2Page geometry . device2Desktop geometry) pcoord 
                 guard (pnum == pnum') 
                 guard (isPointInBBox bbox (x,y))
                 case ritem of
                   RItemLink lnkbbx _ -> do 
                     forM_ ((urlParse . B.unpack .  link_location . bbxed_content) lnkbbx)
                           (liftIO . openLinkAction)
                     liftIO $ putStrLn "I am in" 
                     MaybeT (return (Just ()))
                   _ -> MaybeT (return Nothing))
      case m of        
        Nothing -> defact 
        Just _ -> return ()
