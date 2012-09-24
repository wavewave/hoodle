{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.Generic 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--

module Graphics.Hoodle.Render.Generic where

import Control.Lens
import Data.Foldable
import Graphics.Rendering.Cairo
-- from hoodle-platform
import Data.Hoodle.BBox
import Data.Hoodle.Generic
import Data.Hoodle.Simple
-- from this package 
import Graphics.Hoodle.Render
import Graphics.Hoodle.Render.Type 
-- import Graphics.Hoodle.Render.Simple
-- import Graphics.Hoodle.Render.BBox
-- 
import Prelude hiding (mapM_)

-- | 
class Renderable a where 
  cairoRender :: a -> Render ()
                 
-- | 
instance Renderable (Background,Dimension) where
  cairoRender = renderBkg 

-- | 
instance Renderable Stroke where 
  cairoRender = renderStrk 

-- | 
instance Renderable StrokeBBox where
  cairoRender = renderStrk . strkbbx_strk
  
-- | 
instance Renderable RLayer where
  cairoRender = renderRLayer_InBBox Nothing 
  
{-  
-- | 
instance Renderable RPage where
  cairoRender = renderRPage 
-}

-- | 
class RenderOptionable a where   
  type RenderOption a :: *
  cairoRenderOption :: RenderOption a -> a -> Render ()

-- | 
instance RenderOptionable (Background,Dimension) where
  type RenderOption (Background,Dimension) = ()
  cairoRenderOption () = cairoRender 

-- | 
instance RenderOptionable Stroke where
  type RenderOption Stroke = () 
  cairoRenderOption () = cairoRender
  
-- | 
data StrokeBBoxOption = DrawFull | DrawBoxOnly 

-- | 
instance RenderOptionable StrokeBBox where
  type RenderOption StrokeBBox = StrokeBBoxOption
  cairoRenderOption DrawFull = cairoRender 
  cairoRenderOption DrawBoxOnly = renderStrkBBx_BBoxOnly
  


-- |
cairoOptionPage :: ( RenderOptionable (b,Dimension)
                   , RenderOptionable a
                   , Foldable s) => 
                   (RenderOption (b,Dimension), RenderOption a) 
                   -> GPage b s a 
                   -> Render ()
cairoOptionPage (optb,opta) p = do 
    cairoRenderOption optb (view gbackground p, view gdimension p)
    mapM_ (cairoRenderOption opta) (view glayers p)
  
-- | 
instance ( RenderOptionable (b,Dimension)
         , RenderOptionable a
         , Foldable s) =>
         RenderOptionable (GPage b s a) where
  type RenderOption (GPage b s a) = (RenderOption (b,Dimension), RenderOption a)
  cairoRenderOption = cairoOptionPage
            

{-
-- |   
cairoLayer :: (Renderable a, Foldable s) => GLayer buf s a -> Render ()
cairoLayer = mapM_ cairoRender . gstrokes 

-- | 
instance (Renderable a, Foldable s) => Renderable (GLayer s a) where
  cairoRender = cairoLayer 

-- | 
cairoPage :: (Renderable (b,Dimension), Renderable a, Foldable s) =>  
             GPage b s a -> Render ()
cairoPage p = do 
  cairoRender (gbackground p,gdimension p)
  mapM_ cairoRender (glayers p)

-- | 
instance (Renderable (b,Dimension), Renderable a, Foldable s) 
         => Renderable (GPage b s a) where
  cairoRender = cairoPage 
-}



{-
-- | 
cairoOptionLayer :: (RenderOptionable a, Foldable s) => 
                    RenderOption a -> GLayer s a -> Render () 
cairoOptionLayer opt = mapM_ (cairoRenderOption opt) . gstrokes 

-- | 
instance (RenderOptionable a, Foldable s) => 
         RenderOptionable (GLayer s a) where
  type RenderOption (GLayer s a) = RenderOption a
  cairoRenderOption = cairoOptionLayer 
-}

