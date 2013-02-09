{-# LANGUAGE GADTs, RankNTypes, TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Link
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Link where

import           Control.Applicative
import           Control.Category
import           Control.Lens
import           Control.Monad.State 
-- import           Control.Monad.Trans
import qualified Data.IntMap as M
import           Graphics.UI.Gtk hiding (get,set) -- (adjustmentGetValue)
-- from hoodle-platform
-- import           Control.Monad.Trans.Crtn.Event 
-- import           Control.Monad.Trans.Crtn.Queue 
import           Data.Hoodle.BBox
import           Data.Hoodle.Generic
import           Data.Hoodle.Select
import           Graphics.Hoodle.Render
import           Graphics.Hoodle.Render.Type 
import           Graphics.Hoodle.Render.Type.HitTest 
import           Graphics.Hoodle.Render.Util.HitTest 
-- from this package
import           Hoodle.Accessor
import           Hoodle.Coroutine.Draw
import           Hoodle.Device 
import           Hoodle.Type.Alias
import           Hoodle.Type.Canvas
import           Hoodle.Type.Coroutine
-- import           Hoodle.Type.Enum 
import           Hoodle.Type.Event
import           Hoodle.Type.HoodleState
import           Hoodle.Type.PageArrangement
import           Hoodle.View.Coordinate
import           Hoodle.View.Draw
--
import Prelude hiding ((.),id, mapM_, mapM)

notifyLink :: CanvasId -> PointerCoord -> MainCoroutine () 
notifyLink cid pcoord = do 
    xst <- get 
    (boxAction f . getCanvasInfo cid) xst 
  where 
    f :: forall b. (ViewMode b) => CanvasInfo b -> MainCoroutine () 
    f cvsInfo = do 
      let cpn = PageNum . view currentPageNum $ cvsInfo
          arr = view (viewInfo.pageArrangement) cvsInfo              
          canvas = view drawArea cvsInfo
      geometry <- liftIO $ makeCanvasGeometry cpn arr canvas
      case (desktop2Page geometry . device2Desktop geometry) pcoord of
        Nothing -> return () 
        Just (pnum,PageCoord (x,y)) -> do 
          itms <- rItmsInCurrLyr    
          let lnks = filter isLinkInRItem itms           
              hlnks = hltFilteredBy (\itm->isPointInBBox (getBBox itm) (x,y)) lnks
              hitted = takeHitted hlnks 
          when ((not.null) hitted) $ do  
            let lnk = head hitted 
                bbx = getBBox lnk
                bbx_desk = xformBBox (unDeskCoord . page2Desktop geometry
                                      . (pnum,) . PageCoord) bbx
            invalidateInBBox (Just bbx_desk) Efficient cid 

            
                
            




