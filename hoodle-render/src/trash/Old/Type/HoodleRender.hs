
-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.SimpleNew
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Graphics.Hoodle.Render.Type.HoodleRender where 

import Control.Monad.State 
import Graphics.Rendering.Cairo


-- |
type HoodleRender = StateT () Render 
