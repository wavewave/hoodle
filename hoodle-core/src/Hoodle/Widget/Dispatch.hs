{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Widget.Dispatch 
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Widget.Dispatch where

import Control.Applicative ((<|>))
import Control.Lens (view)
import Control.Monad.State hiding (forM_)
import Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as TE
import Data.Foldable (forM_)
--
import Data.Hoodle.BBox
import Data.Hoodle.Simple 
import Graphics.Hoodle.Render.Util.HitTest
import Graphics.Hoodle.Render.Type.Item
-- 
import Hoodle.Coroutine.Link
import Hoodle.Device 
import Hoodle.Type.Canvas
import Hoodle.Type.Coroutine
import Hoodle.Type.HoodleState
import Hoodle.Type.PageArrangement
import Hoodle.Util
import Hoodle.View.Coordinate 
import Hoodle.Widget.Clock
import Hoodle.Widget.Layer
import Hoodle.Widget.PanZoom

widgetCheckPen :: CanvasId 
               -> PointerCoord 
               -> MainCoroutine ()    -- ^ default action 
               -> MainCoroutine ()
widgetCheckPen cid pcoord defact = 
    get >>= \xst -> forBoth' unboxBiAct (chk xst) ((getCanvasInfo cid . view (unitHoodles.currentUnit)) xst)
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
                 (pnum',bbox,ritem) <- (MaybeT . return . view notifiedItem) cinfo
                 (pnum'',PageCoord (x,y)) <- (MaybeT . return . desktop2Page geometry . device2Desktop geometry) pcoord 
                 guard (pnum' == pnum'') 
                 guard (isPointInBBox bbox (x,y))
                 case ritem of
                   RItemLink lnkbbx _ -> do 
                     let lnk = bbxed_content lnkbbx
                         loc = link_location lnk
                         mid = case lnk of 
                           LinkAnchor {..} -> Just (TE.decodeUtf8 link_linkeddocid,TE.decodeUtf8 link_anchorid)
                           _ -> Nothing
                     forM_  ((urlParse . B.unpack) loc) (\url -> lift (openLinkAction url mid))
                     MaybeT (return (Just ()))
                   _ -> MaybeT (return Nothing))
      case m of        
        Nothing -> defact 
        Just _ -> return ()
