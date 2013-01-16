{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.Generic 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Graphics.Hoodle.Render.Generic where

import Control.Applicative
import Control.Lens
import Control.Monad hiding (mapM_,mapM)
import Data.Foldable
import Data.Traversable 
import Graphics.Rendering.Cairo
-- from hoodle-platform
import Data.Hoodle.BBox
import Data.Hoodle.Generic
import Data.Hoodle.Simple
-- from this package 
import Graphics.Hoodle.Render
import Graphics.Hoodle.Render.Debug
import Graphics.Hoodle.Render.Type 
-- 
import Prelude hiding (mapM_,mapM)

-- | temporary util
passarg :: (Monad m) => (a -> m ()) -> a -> m a
passarg f a = f a >> return a

-- | 
class Renderable a where 
  cairoRender :: a -> Render a
                 
-- | 
instance Renderable (Background,Dimension) where
  cairoRender = passarg renderBkg 

-- | 
instance Renderable Stroke where 
  cairoRender = passarg renderStrk 

-- | 
instance Renderable StrokeBBox where
  cairoRender = passarg (renderStrk . strkbbx_strk)
  
-- | 
instance Renderable RLayer where
  cairoRender = renderRLayer_InBBox Nothing 
  
-- | 
class RenderOptionable a where   
  type RenderOption a :: *
  cairoRenderOption :: RenderOption a -> a -> Render a

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
  cairoRenderOption DrawBoxOnly = passarg renderStrkBBx_BBoxOnly
  
-- | 
instance RenderOptionable (RBackground,Dimension) where 
  type RenderOption (RBackground,Dimension) = RBkgOpt 
  -- cairoRenderOption :: RBkgOpt -> (RBackground,Dimension) -> Render ()
  cairoRenderOption RBkgDrawPDF = renderRBkg
  cairoRenderOption RBkgDrawWhite = passarg renderRBkg_Dummy
  cairoRenderOption RBkgDrawBuffer = renderRBkg_Buf 
  cairoRenderOption (RBkgDrawPDFInBBox mbbox) = renderRBkg_InBBox mbbox 

-- | 
instance RenderOptionable RLayer where
  type RenderOption RLayer = StrokeBBoxOption 
  cairoRenderOption DrawFull = cairoRender
  cairoRenderOption DrawBoxOnly = passarg renderRLayer_BBoxOnly 

-- | 
instance RenderOptionable (InBBox RLayer) where
  type RenderOption (InBBox RLayer) = InBBoxOption
  cairoRenderOption (InBBoxOption mbbox) (InBBox lyr) = 
    InBBox <$> renderRLayer_InBBoxBuf mbbox lyr
    
-- |
cairoOptionPage :: ( RenderOptionable (b,Dimension)
                   , RenderOptionable a
                   , Foldable s) => 
                   (RenderOption (b,Dimension), RenderOption a) 
                   -> GPage b s a 
                   -> Render (GPage b s a)
cairoOptionPage (optb,opta) p = do 
    cairoRenderOption optb (view gbackground p, view gdimension p)
    mapM_ (cairoRenderOption opta) (view glayers p)
    return p 
  
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
    cairoRenderOption (RBkgDrawPDFInBBox mbbox) (view gbackground page, view gdimension page) 
    --  mapM_ (renderRLayer_InBBox mbbox) . view glayers $ page 
    let lyrs = view glayers page
    nlyrs <- mapM (liftM unInBBox . cairoRenderOption (InBBoxOption mbbox) . InBBox ) lyrs
    let npage = set glayers nlyrs page
    return (InBBox npage) 

-- | 
instance RenderOptionable (InBBoxBkgBuf RPage) where
  type RenderOption (InBBoxBkgBuf RPage) = InBBoxOption 
  cairoRenderOption (InBBoxOption mbbox) (InBBoxBkgBuf page) = do 
    cairoRenderOption (RBkgDrawPDFInBBox mbbox) (view gbackground page, view gdimension page) 
    --  mapM_ (renderRLayer_InBBox mbbox) . view glayers $ page 
    let lyrs = view glayers page
    nlyrs <- mapM (renderRLayer_InBBox mbbox) lyrs
    let npage = set glayers nlyrs page
    return (InBBoxBkgBuf npage) 



