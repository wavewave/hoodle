{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Graphics.Hoodle.Render.Generic where

import Control.Lens (set, view, _1)
import Data.Hoodle.BBox
  ( BBoxed (..),
  )
import Data.Hoodle.Generic
  ( GPage,
    gbackground,
    gdimension,
    glayers,
  )
import Data.Hoodle.Simple
  ( Background,
    Dimension,
    Stroke,
  )
import Graphics.Hoodle.Render
  ( Xform4Page,
    renderBkg,
    renderRBkg,
    renderRBkgBuf,
    renderRBkgInBBox,
    renderRLayerInBBox,
    renderRLayerInBBoxBuf,
    renderStrk,
  )
import Graphics.Hoodle.Render.Type
  ( CanvasId,
    InBBox (..),
    InBBoxBkgBuf (..),
    InBBoxOption (..),
    RBackground (..),
    RBkgOpt (..),
    RLayer,
    RPage,
    RenderCache,
  )
import qualified Graphics.Rendering.Cairo as Cairo

-- | temporary util
passarg :: (Monad m) => (CanvasId -> a -> m ()) -> CanvasId -> a -> m a
passarg f i a = f i a >> return a

passarg1 :: (Monad m) => (a -> m ()) -> a -> m a
passarg1 f a = f a >> return a

const2 :: c -> a -> b -> c
const2 f _x _y = f

-- |
class Renderable a where
  cairoRender :: RenderCache -> CanvasId -> a -> Cairo.Render a

-- |
instance Renderable (Background, Dimension) where
  cairoRender = const2 (passarg1 renderBkg)

-- |
instance Renderable Stroke where
  cairoRender = const2 (passarg1 renderStrk)

-- |
instance Renderable (BBoxed Stroke) where
  cairoRender = const2 (passarg1 (renderStrk . bbxed_content))

-- |
instance Renderable (RLayer, Dimension, Maybe Xform4Page) where
  cairoRender cache cid = renderRLayerInBBox cache cid Nothing

-- |
class RenderOptionable a where
  type RenderOption a :: *
  cairoRenderOption :: RenderOption a -> RenderCache -> CanvasId -> a -> Cairo.Render a

-- |
instance RenderOptionable (Background, Dimension) where
  type RenderOption (Background, Dimension) = ()
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
  cairoRenderOption DrawBoxOnly = error "BBoxed Stroke.cairoRenderOption: DrawBoxOnly deprecated"

-- |
instance RenderOptionable (RBackground, Dimension, Maybe Xform4Page) where
  type RenderOption (RBackground, Dimension, Maybe Xform4Page) = RBkgOpt
  cairoRenderOption RBkgDrawPDF cache cid = renderRBkg cache cid
  cairoRenderOption RBkgDrawWhite _cache _cid = error "RBackground...cairoRenderOption: RBkgDrawWhite deprecated"
  cairoRenderOption RBkgDrawBuffer cache cid = renderRBkgBuf cache cid
  cairoRenderOption (RBkgDrawPDFInBBox mbbox) cache cid = renderRBkgInBBox cache cid mbbox

-- |
instance RenderOptionable (RLayer, Dimension, Maybe Xform4Page) where
  type RenderOption (RLayer, Dimension, Maybe Xform4Page) = StrokeBBoxOption
  cairoRenderOption DrawFull cache cid = cairoRender cache cid
  cairoRenderOption DrawBoxOnly _cache _cid = error "RLayer.cairoRenderOption: DrawBoxOnly deprecated"

-- |
instance RenderOptionable (InBBox (RLayer, Dimension, Maybe Xform4Page)) where
  type RenderOption (InBBox (RLayer, Dimension, Maybe Xform4Page)) = InBBoxOption
  cairoRenderOption (InBBoxOption mbbox) cache cid (InBBox lyrinfo) =
    InBBox <$> renderRLayerInBBoxBuf cache cid mbbox lyrinfo

-- |
cairoOptionPage ::
  ( RenderOptionable (b, Dimension, Maybe Xform4Page),
    RenderOptionable (a, Dimension, Maybe Xform4Page),
    Foldable s,
    Functor s
  ) =>
  ( RenderOption (b, Dimension, Maybe Xform4Page),
    RenderOption (a, Dimension, Maybe Xform4Page)
  ) ->
  RenderCache ->
  CanvasId ->
  (GPage b s a, Maybe Xform4Page) ->
  Cairo.Render (GPage b s a, Maybe Xform4Page)
cairoOptionPage (optb, opta) cache cid (p, mx) = do
  let (bkg, dim) = (view gbackground p, view gdimension p)
  _ <- cairoRenderOption optb cache cid (bkg, dim, mx)
  mapM_ (cairoRenderOption opta cache cid) . fmap (,dim,mx) $ view glayers p
  return (p, mx)

-- |
instance
  ( RenderOptionable (b, Dimension, Maybe Xform4Page),
    RenderOptionable (a, Dimension, Maybe Xform4Page),
    Foldable s,
    Functor s
  ) =>
  RenderOptionable (GPage b s a, Maybe Xform4Page)
  where
  type RenderOption (GPage b s a, Maybe Xform4Page) = (RenderOption (b, Dimension, Maybe Xform4Page), RenderOption (a, Dimension, Maybe Xform4Page))
  cairoRenderOption = cairoOptionPage

-- |
instance RenderOptionable (InBBox RPage, Maybe Xform4Page) where
  type RenderOption (InBBox RPage, Maybe Xform4Page) = InBBoxOption
  cairoRenderOption (InBBoxOption mbbox) cache cid (InBBox page, mx) = do
    let (bkg, dim) = (view gbackground page, view gdimension page)
    _ <- cairoRenderOption (RBkgDrawPDFInBBox mbbox) cache cid (bkg, dim, mx)
    let lyrs = view glayers page
    nlyrs <- mapM (fmap unInBBox . cairoRenderOption (InBBoxOption mbbox) cache cid . InBBox) . fmap (,dim,mx) $ lyrs
    let npage = set glayers (fmap (view _1) nlyrs) page
    return (InBBox npage, mx)

-- |
instance RenderOptionable (InBBoxBkgBuf RPage, Maybe Xform4Page) where
  type RenderOption (InBBoxBkgBuf RPage, Maybe Xform4Page) = InBBoxOption
  cairoRenderOption (InBBoxOption mbbox) cache cid (InBBoxBkgBuf page, mx) = do
    let bkg = view gbackground page
        dim = view gdimension page
    _ <- cairoRenderOption (RBkgDrawPDFInBBox mbbox) cache cid (bkg, dim, mx)
    let lyrs = view glayers page
    nlyrs <- mapM (renderRLayerInBBox cache cid mbbox) . fmap (,dim,mx) $ lyrs
    let npage = set glayers (fmap (view _1) nlyrs) page
    return (InBBoxBkgBuf npage, mx)
