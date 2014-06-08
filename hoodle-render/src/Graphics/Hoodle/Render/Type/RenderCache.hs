-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.Type.RenderCache 
-- Copyright   : (c) 2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Graphics.Hoodle.Render.Type.RenderCache where

import qualified Data.HashMap.Strict as HM
import           Data.UUID
import qualified Graphics.Rendering.Cairo as Cairo

type RenderCache = HM.HashMap UUID (Maybe Cairo.Surface)
