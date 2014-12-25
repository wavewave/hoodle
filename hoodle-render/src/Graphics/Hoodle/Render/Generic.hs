{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.Generic 
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
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
import qualified Graphics.Rendering.Cairo as Cairo
-- from hoodle-platform
import Data.Hoodle.BBox
import Data.Hoodle.Generic
import Data.Hoodle.Simple
-- from this package 
import Graphics.Hoodle.Render
-- import Graphics.Hoodle.Render.Debug
import Graphics.Hoodle.Render.Type 
-- 
import Prelude hiding (mapM_,mapM)

-- | temporary util
passarg :: (Monad m) => (CanvasId -> a -> m ()) -> CanvasId -> a -> m a
passarg f i a = f i a >> return a


passarg1 :: (Monad m) => (a -> m ()) -> a -> m a
passarg1 f a = f a >> return a

const2 :: c -> a -> b -> c
const2 f x y = f

-- | 
class Renderable a where 
  cairoRender :: RenderCache -> CanvasId -> a -> Cairo.Render a
                 
-- | 
instance Renderable (Background,Dimension) where
  cairoRender = const2 (passarg1 renderBkg) 

-- | 
instance Renderable Stroke where 
  cairoRender = const2 (passarg1 renderStrk)

-- | 
instance Renderable (BBoxed Stroke) where
  cairoRender =const2 (passarg1 (renderStrk . bbxed_content))
  
-- | 
instance Renderable RLayer where
  cairoRender cache cid = renderRLayer_InBBox cache cid Nothing 
  
-- | 
class RenderOptionable a where   
  type RenderOption a :: *
  cairoRenderOption :: RenderOption a -> RenderCache -> CanvasId -> a -> Cairo.Render a

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
instance RenderOptionable (BBoxed Stroke) where
  type RenderOption (BBoxed Stroke) = StrokeBBoxOption
  cairoRenderOption DrawFull = cairoRender 
  cairoRenderOption DrawBoxOnly = error "BBoxed Stroke.cairoRenderOption: DrawBoxOnly deprecated" -- const (passarg renderStrkBBx_BBoxOnly)
  
-- | 
instance RenderOptionable (RBackground,Dimension,Maybe Xform4Page) where 
  type RenderOption (RBackground,Dimension,Maybe Xform4Page) = RBkgOpt 
  cairoRenderOption RBkgDrawPDF cache cid = renderRBkg cache cid
  cairoRenderOption RBkgDrawWhite cache cid = error "RBackground...cairoRenderOption: RBkgDrawWhite deprecated" -- passarg (renderRBkg_Dummy cache)
  cairoRenderOption RBkgDrawBuffer cache cid = renderRBkg_Buf cache cid 
  cairoRenderOption (RBkgDrawPDFInBBox mbbox) cache cid = renderRBkg_InBBox cache cid mbbox 

-- | 
instance RenderOptionable RLayer where
  type RenderOption RLayer = StrokeBBoxOption 
  cairoRenderOption DrawFull cache cid = cairoRender cache cid
  cairoRenderOption DrawBoxOnly cache cid = error "RLayer.cairoRenderOption: DrawBoxOnly deprecated" -- passarg (renderRLayer_BBoxOnly cache)

-- | 
instance RenderOptionable (InBBox RLayer) where
  type RenderOption (InBBox RLayer) = InBBoxOption
  cairoRenderOption (InBBoxOption mbbox) cache cid (InBBox lyr) = 
    InBBox <$> renderRLayer_InBBoxBuf cache cid mbbox lyr
    
-- |
cairoOptionPage :: ( RenderOptionable (b,Dimension,Maybe Xform4Page)
                   , RenderOptionable a
                   , Foldable s) => 
                   (RenderOption (b,Dimension,Maybe Xform4Page), RenderOption a) 
                   -> RenderCache
                   -> CanvasId
                   -> (GPage b s a, Maybe Xform4Page)
                   -> Cairo.Render (GPage b s a, Maybe Xform4Page)
cairoOptionPage (optb,opta) cache cid (p,mx) = do 
    cairoRenderOption optb cache cid (view gbackground p, view gdimension p,mx)
    mapM_ (cairoRenderOption opta cache cid) (view glayers p)
    return (p,mx) 
  
-- | 
instance ( RenderOptionable (b,Dimension,Maybe Xform4Page)
         , RenderOptionable a
         , Foldable s) =>
         RenderOptionable (GPage b s a,Maybe Xform4Page) where
  type RenderOption (GPage b s a,Maybe Xform4Page) = (RenderOption (b,Dimension,Maybe Xform4Page), RenderOption a)
  cairoRenderOption = cairoOptionPage
            
-- | 
instance RenderOptionable (InBBox RPage,Maybe Xform4Page) where
  type RenderOption (InBBox RPage,Maybe Xform4Page) = InBBoxOption 
  cairoRenderOption (InBBoxOption mbbox) cache cid (InBBox page,mx) = do 
    cairoRenderOption (RBkgDrawPDFInBBox mbbox) cache cid (view gbackground page, view gdimension page, mx)
    let lyrs = view glayers page
    nlyrs <- mapM (liftM unInBBox . cairoRenderOption (InBBoxOption mbbox) cache cid . InBBox ) lyrs
    let npage = set glayers nlyrs page
    return (InBBox npage,mx) 

-- | 
instance RenderOptionable (InBBoxBkgBuf RPage,Maybe Xform4Page) where
  type RenderOption (InBBoxBkgBuf RPage,Maybe Xform4Page) = InBBoxOption 
  cairoRenderOption (InBBoxOption mbbox) cache cid (InBBoxBkgBuf page,mx) = do 
    cairoRenderOption (RBkgDrawPDFInBBox mbbox) cache cid (view gbackground page, view gdimension page, mx)
    let lyrs = view glayers page
    nlyrs <- mapM (renderRLayer_InBBox cache cid mbbox) lyrs
    let npage = set glayers nlyrs page
    return (InBBoxBkgBuf npage,mx) 



