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
-----------------------------------------------------------------------------

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
instance RenderOptionable (RBackground,Dimension) where 
  type RenderOption (RBackground,Dimension) = RBkgOpt 
  -- cairoRenderOption :: RBkgOpt -> (RBackground,Dimension) -> Render ()
  cairoRenderOption RBkgDrawPDF = renderRBkg
  cairoRenderOption RBkgDrawWhite = renderRBkg_Dummy
  cairoRenderOption RBkgDrawBuffer = renderRBkg_Buf 
  cairoRenderOption (RBkgDrawPDFInBBox mbbox) = renderRBkg_InBBox mbbox 

-- | 
instance RenderOptionable RLayer where
  type RenderOption RLayer = StrokeBBoxOption 
  cairoRenderOption DrawFull = cairoRender
  cairoRenderOption DrawBoxOnly = renderRLayer_BBoxOnly 

-- | 
instance RenderOptionable (InBBox RLayer) where
  type RenderOption (InBBox RLayer) = InBBoxOption
  -- cairoRenderOption :: RLyrOpt -> RLayer -> Render ()  
  cairoRenderOption (InBBoxOption mbbox) (InBBox lyr) = 
    renderRLayer_InBBoxBuf mbbox lyr
    
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
            
-- | 
instance RenderOptionable (InBBox RPage) where
  type RenderOption (InBBox RPage) = InBBoxOption 
  cairoRenderOption (InBBoxOption mbbox) (InBBox page) = do 
    let mbboxtemp = mbbox >>= \bbox -> return (inflate bbox 2.0) -- this is due to a thin unexpected grey margin. 
    cairoRenderOption (RBkgDrawPDFInBBox mbboxtemp) (view gbackground page, view gdimension page) 
    mapM_ (renderRLayer_InBBox mbbox) . view glayers $ page 





