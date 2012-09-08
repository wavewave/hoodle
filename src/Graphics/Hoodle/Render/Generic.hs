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

import Data.Foldable
import Prelude hiding (mapM_)

-- import Text.Hoodle.Type

import Data.Hoodle.Generic
import Data.Hoodle.Simple
import Data.Hoodle.BBox
import Graphics.Rendering.Cairo
import Graphics.Hoodle.Render.Simple
import Graphics.Hoodle.Render.BBox

class Renderable a where 
  cairoRender :: a -> Render ()
                 
instance Renderable (Background,Dimension) where
  cairoRender (b,dim) = cairoDrawBkg dim b 

instance Renderable Stroke where 
  cairoRender = drawOneStroke 

instance Renderable StrokeBBox where
  cairoRender = drawOneStroke . gToStroke 
  
cairoLayer :: (Renderable a, Foldable s) => GLayer s a -> Render ()
cairoLayer = mapM_ cairoRender . gstrokes 

instance (Renderable a, Foldable s) => Renderable (GLayer s a) where
  cairoRender = cairoLayer 

cairoPage :: (Renderable (b,Dimension), Renderable a, Foldable s) =>  
             GPage b s a -> Render ()
cairoPage p = do 
  cairoRender (gbackground p,gdimension p)
  mapM_ cairoRender (glayers p)

instance (Renderable (b,Dimension), Renderable a, Foldable s) 
         => Renderable (GPage b s a) where
  cairoRender = cairoPage 


class RenderOptionable a where   
  type RenderOption a :: *
  cairoRenderOption :: RenderOption a -> a -> Render ()

instance RenderOptionable (Background,Dimension) where
  type RenderOption (Background,Dimension) = ()
  cairoRenderOption () = cairoRender 

instance RenderOptionable Stroke where
  type RenderOption Stroke = () 
  cairoRenderOption () = cairoRender
  
data StrokeBBoxOption = DrawFull | DrawBoxOnly 

instance RenderOptionable StrokeBBox where
  type RenderOption StrokeBBox = StrokeBBoxOption
  cairoRenderOption DrawFull = cairoRender 
  cairoRenderOption DrawBoxOnly = cairoOneStrokeBBoxOnly
  
cairoOptionLayer :: (RenderOptionable a, Foldable s) => 
                    RenderOption a -> GLayer s a -> Render () 
cairoOptionLayer opt = mapM_ (cairoRenderOption opt) . gstrokes 

instance (RenderOptionable a, Foldable s) => 
         RenderOptionable (GLayer s a) where
  type RenderOption (GLayer s a) = RenderOption a
  cairoRenderOption = cairoOptionLayer 

cairoOptionPage :: ( RenderOptionable (b,Dimension)
                   , RenderOptionable a
                   , Foldable s) => 
                   (RenderOption (b,Dimension), RenderOption a) 
                   -> GPage b s a 
                   -> Render ()
cairoOptionPage (optb,opta) p = do 
    cairoRenderOption optb (gbackground p, gdimension p)
    mapM_ (cairoRenderOption opta) (glayers p)
  
instance ( RenderOptionable (b,Dimension)
         , RenderOptionable a
         , Foldable s) =>
         RenderOptionable (GPage b s a) where
  type RenderOption (GPage b s a) = (RenderOption (b,Dimension), RenderOption a)
  cairoRenderOption = cairoOptionPage
            
