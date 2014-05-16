{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render 
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- collection of rendering routine 
--
-----------------------------------------------------------------------------

module Graphics.Hoodle.Render 
(      
-- * simple rendering using non-R-structure   
  renderStrk
, renderImg
, renderBkg
, renderItem 
, renderPage
-- * simple rendering using R-structure 
, renderRBkg
, renderRItem 
-- * render in bbox
, renderRLayer_InBBox
, renderRBkg_InBBox 
-- * render using buf 
, renderRBkg_Buf
, renderRLayer_InBBoxBuf
-- * buffer update 
, updateLayerBuf
, updatePageBuf 
, updateHoodleBuf 
-- * construct R-structure from non-R-structure 
, cnstrctRLayer
, cnstrctRBkg_StateT
, cnstrctRPage_StateT
, cnstrctRHoodle  
) where

import           Control.Lens (view,set)
import           Control.Monad.Identity (runIdentity)
import           Control.Monad.State hiding (mapM,mapM_)
import qualified Data.ByteString.Char8 as C
import           Data.Foldable
import           Data.Traversable (mapM)
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Cairo.SVG as RSVG
import qualified Graphics.UI.Gtk.Poppler.Page as PopplerPage
-- from hoodle-platform 
import           Data.Hoodle.Generic
import           Data.Hoodle.Simple
import           Data.Hoodle.BBox
import           Data.Hoodle.Predefined 
import           Data.Hoodle.Zipper 
-- from this package
import Graphics.Hoodle.Render.Background
-- import Graphics.Hoodle.Render.Highlight
import Graphics.Hoodle.Render.Item 
import Graphics.Hoodle.Render.Primitive
import Graphics.Hoodle.Render.Type 
import Graphics.Hoodle.Render.Type.HitTest
import Graphics.Hoodle.Render.Util 
import Graphics.Hoodle.Render.Util.HitTest 
-- 
import Prelude hiding (curry,uncurry,mapM,mapM_,concatMap)

------------
-- simple --
------------

-- | render stroke 
renderStrk :: Stroke -> Cairo.Render ()
renderStrk s@(Stroke _ _ w d) = do 
    let opacity = if stroke_tool s == "highlighter" 
                  then predefined_highlighter_opacity
                  else 1.0
    case getPenColor (stroke_color s) of 
      Just (r,g,b,a) -> Cairo.setSourceRGBA r g b (a*opacity) 
      Nothing -> Cairo.setSourceRGBA 0.5 0.5 0.5 1
    Cairo.setLineWidth w
    Cairo.setLineCap Cairo.LineCapRound
    Cairo.setLineJoin Cairo.LineJoinRound
    drawStrokeCurve d
    Cairo.stroke 
renderStrk s@(VWStroke _ _ d) = do 
    let opacity = if stroke_tool s == "highlighter" 
                  then predefined_highlighter_opacity
                  else 1.0
    case getPenColor (stroke_color s) of
      Just (r,g,b,a) -> Cairo.setSourceRGBA r g b (a*opacity) 
      Nothing -> Cairo.setSourceRGBA 0.5 0.5 0.5 1
    Cairo.setFillRule Cairo.FillRuleWinding
    drawVWStrokeCurve d
    Cairo.fill 

-- | render image : not fully implemented 
renderImg :: Image -> Cairo.Render () 
renderImg (Image _ (x,y) (Dim w h)) = do  
      Cairo.setSourceRGBA 0 0 0 1
      Cairo.setLineWidth 10
      Cairo.rectangle x y w h
      Cairo.stroke

-- | render svg  
renderSVG :: SVG -> Cairo.Render () 
renderSVG svg@(SVG _ _ bstr _ _) = do  
    let str = C.unpack bstr 
    RSVG.withSvgFromString str $ \rsvg -> do 
      let svgbbx = runIdentity (makeBBoxed svg)
      let (x,y) = (svg_pos . bbxed_content) svgbbx
          BBox (x1,y1) (x2,y2) = getBBox svgbbx
          (ix',iy') = RSVG.svgGetSize rsvg
          ix = fromIntegral ix' 
          iy = fromIntegral iy'
      Cairo.save 
      Cairo.translate x y 
      Cairo.scale ((x2-x1)/ix) ((y2-y1)/iy)
      RSVG.svgRender rsvg 
      Cairo.restore
      return () 

-- | render svg  
renderLink :: Link -> Cairo.Render () 
renderLink lnk@LinkAnchor {..} = do
    let lnkbbx = runIdentity (makeBBoxed lnk)
        bbox@(BBox (x0,y0) (x1,y1)) = getBBox lnkbbx
    clipBBox (Just bbox)
    Cairo.setSourceRGBA 0 1 0 1 
    Cairo.rectangle x0 y0 (x1-x0) (y1-y0)
    Cairo.fill
    Cairo.resetClip
    return ()
renderLink lnk = do  
    let bstr = link_render lnk 
    let str = C.unpack bstr 
    RSVG.withSvgFromString str $ \rsvg -> do 
      let lnkbbx = runIdentity (makeBBoxed lnk)
      let (x,y) = (link_pos . bbxed_content) lnkbbx
          BBox (x1,y1) (x2,y2) = getBBox lnkbbx
          (ix',iy') = RSVG.svgGetSize rsvg
          ix = fromIntegral ix' 
          iy = fromIntegral iy'
      clipBBox (Just (getBBox lnkbbx))
      Cairo.save 
      Cairo.translate x y 
      Cairo.scale ((x2-x1)/ix) ((y2-y1)/iy)
      RSVG.svgRender rsvg 
      Cairo.restore
      Cairo.resetClip 
      return () 


-- | 
renderAnchor :: Anchor -> Cairo.Render ()
renderAnchor anc = do
    let ancbbx = runIdentity (makeBBoxed anc)
        bbox@(BBox (x0,y0) (x1,y1)) = getBBox ancbbx
    clipBBox (Just bbox)
    Cairo.setSourceRGBA 1 0 0 1 
    Cairo.rectangle x0 y0 (x1-x0) (y1-y0)
    Cairo.fill
    Cairo.resetClip
    return ()

-- | render item 
renderItem :: Item -> Cairo.Render () 
renderItem (ItemStroke strk) = renderStrk strk
renderItem (ItemImage img) = renderImg img
renderItem (ItemSVG svg) = renderSVG svg
renderItem (ItemLink lnk) = renderLink lnk
renderItem (ItemAnchor anc) = renderAnchor anc 

-- |
renderPage :: Page -> Cairo.Render ()
renderPage page = do 
  renderBkg (view background page,view dimension page)
  Cairo.setLineCap Cairo.LineCapRound
  Cairo.setLineJoin Cairo.LineJoinRound
  mapM_ (mapM renderItem . view items) . view layers $ page
  Cairo.stroke

-----
-- R-structure 
----

drawFallBackBkg :: Dimension -> Cairo.Render () 
drawFallBackBkg (Dim w h) = do 
  Cairo.setSourceRGBA 1 1 1 1 
  Cairo.rectangle 0 0 w h 
  Cairo.fill 
  Cairo.setSourceRGBA 0 0 0 1 
  Cairo.setLineWidth 5
  Cairo.moveTo 0 0
  Cairo.lineTo w h 
  Cairo.stroke 
  Cairo.moveTo w 0 
  Cairo.lineTo 0 h
  Cairo.stroke
  

-- | 
renderRBkg :: (RBackground,Dimension) -> Cairo.Render (RBackground,Dimension)
renderRBkg (r,dim) = 
    case r of 
      (RBkgSmpl _ _ _) -> 
        drawBkgAndRecord (renderBkg (rbkg2Bkg r,dim))
      (RBkgPDF _ _ _ p _) -> 
        maybe (drawFallBackBkg dim >> return (r,dim)) 
              (\pg -> drawBkgAndRecord (bkgPdfRender pg)) p
      (RBkgEmbedPDF _ p _) -> 
        maybe (drawFallBackBkg dim >> return (r,dim)) 
              (\pg -> drawBkgAndRecord (bkgPdfRender pg)) p 
  where 
    drawBkgAndRecord rdr = do 
      rdr 
      case rbkg_cairosurface r of
        Nothing -> return ()
        Just sfc -> liftIO $ Cairo.renderWith sfc rdr
      return (r,dim)
    bkgPdfRender pg = do 
      let Dim w h = dim 
      Cairo.setSourceRGBA 1 1 1 1
      Cairo.rectangle 0 0 w h 
      Cairo.fill
      PopplerPage.pageRender pg

-- |
renderRItem :: RItem -> Cairo.Render RItem  
renderRItem itm@(RItemStroke strk) = renderStrk (bbxed_content strk) >> return itm
renderRItem itm@(RItemImage img msfc) = do  
    case msfc of
      Nothing -> renderImg (bbxed_content img)
      Just sfc -> do 
	let (x,y) = (img_pos . bbxed_content) img
	    BBox (x1,y1) (x2,y2) = getBBox img
	ix <- liftM fromIntegral (Cairo.imageSurfaceGetWidth sfc)
	iy <- liftM fromIntegral (Cairo.imageSurfaceGetHeight sfc)
	Cairo.save 
	Cairo.translate x y 
	Cairo.scale ((x2-x1)/ix) ((y2-y1)/iy)
	Cairo.setSourceSurface sfc 0 0 
	Cairo.paint 
	Cairo.restore
    return itm 
renderRItem itm@(RItemSVG svgbbx mrsvg) = do 
    case mrsvg of
      Nothing -> renderSVG (bbxed_content svgbbx)
      Just rsvg -> do 
	let (x,y) = (svg_pos . bbxed_content) svgbbx
	    BBox (x1,y1) (x2,y2) = getBBox svgbbx
	    (ix',iy') = RSVG.svgGetSize rsvg
	    ix = fromIntegral ix' 
	    iy = fromIntegral iy'
	Cairo.save 
	Cairo.translate x y 
	Cairo.scale ((x2-x1)/ix) ((y2-y1)/iy)
	RSVG.svgRender rsvg 
	Cairo.restore
	return () 
    return itm 
renderRItem itm@(RItemLink lnkbbx mrsvg) = do 
    case mrsvg of
      Nothing -> renderLink (bbxed_content lnkbbx)
      Just rsvg -> do 
	let (x,y) = (link_pos . bbxed_content) lnkbbx
	    BBox (x1,y1) (x2,y2) = getBBox lnkbbx
	    (ix',iy') = RSVG.svgGetSize rsvg
	    ix = fromIntegral ix' 
	    iy = fromIntegral iy'
	Cairo.save 
	Cairo.translate x y 
	Cairo.scale ((x2-x1)/ix) ((y2-y1)/iy)
	RSVG.svgRender rsvg 
	Cairo.restore
	return () 
    return itm 
renderRItem itm@(RItemAnchor ancbbx) = 
    renderAnchor (bbxed_content ancbbx) >> return itm 

------------
-- InBBox --
------------

-- | background drawing in bbox 
renderRBkg_InBBox :: Maybe BBox 
                  -> (RBackground,Dimension) 
                  -> Cairo.Render (RBackground,Dimension)
renderRBkg_InBBox mbbox (b,dim) = do 
    clipBBox (fmap (flip inflate 1) mbbox)
    renderRBkg_Buf (b,dim)
    Cairo.resetClip
    return (b,dim)


-- | render RLayer within BBox after hittest items
renderRLayer_InBBox :: Maybe BBox -> RLayer -> Cairo.Render RLayer
renderRLayer_InBBox mbbox layer = do  
  clipBBox (fmap (flip inflate 2) mbbox)  -- temporary
  let hittestbbox = case mbbox of 
        Nothing -> NotHitted [] 
                   :- Hitted (view gitems layer) 
                   :- Empty 
        Just bbox -> (hltHittedByBBox bbox . view gitems) layer
  (mapM_ renderRItem . concatMap unHitted  . getB) hittestbbox
  Cairo.resetClip  
  -- simply twice rendering if whole redraw happening 
  case view gbuffer layer of 
    LyBuf (Just sfc) -> do 
      liftIO $ Cairo.renderWith sfc $ do 
        clipBBox (fmap (flip inflate 2) mbbox ) -- temporary
        Cairo.setSourceRGBA 0 0 0 0 
        Cairo.setOperator Cairo.OperatorSource
        Cairo.paint
        Cairo.setOperator Cairo.OperatorOver
        (mapM_ renderRItem . concatMap unHitted  . getB) hittestbbox
        Cairo.resetClip 
        return layer 
    _ -> return layer 

-----------------------
-- draw using buffer -- 
-----------------------

-- | Background rendering using buffer
renderRBkg_Buf :: (RBackground,Dimension) 
               -> Cairo.Render (RBackground,Dimension)
renderRBkg_Buf (b,dim) = do 
    case b of 
      RBkgSmpl _ _ msfc  -> do  
        case msfc of 
          Nothing -> renderRBkg (b,dim) >> return ()
          Just sfc -> do 
            Cairo.save
            Cairo.setSourceSurface sfc 0 0 
            Cairo.paint 
            Cairo.restore
      RBkgPDF _ _ _n _ msfc -> do 
        case msfc of 
          Nothing -> renderRBkg (b,dim) >> return ()
          Just sfc -> do 
            Cairo.save
            Cairo.setSourceSurface sfc 0 0 
            Cairo.paint 
            Cairo.restore
      RBkgEmbedPDF _ _ msfc -> do 
        case msfc of 
          Nothing -> renderRBkg (b,dim) >> return ()
          Just sfc -> do 
            Cairo.save
            Cairo.setSourceSurface sfc 0 0 
            Cairo.paint 
            Cairo.restore
    return (b,dim)

-- | 
renderRLayer_InBBoxBuf :: Maybe BBox -> RLayer -> Cairo.Render RLayer 
renderRLayer_InBBoxBuf mbbox lyr = do
    case view gbuffer lyr of 
      LyBuf (Just sfc) -> do 
        clipBBox mbbox
        Cairo.setSourceSurface sfc 0 0 
        Cairo.paint 
        Cairo.resetClip 
        return lyr 
      _ -> do 
        renderRLayer_InBBox mbbox lyr         

-------------------
-- update buffer
-------------------

-- | 
updateLayerBuf :: Dimension -> Maybe BBox -> RLayer -> IO RLayer
updateLayerBuf (Dim w h) mbbox lyr = do 
  case view gbuffer lyr of 
    LyBuf (Just sfc) -> do 
      Cairo.renderWith sfc $ do 
        renderRLayer_InBBox mbbox lyr 
      return lyr
    LyBuf Nothing -> do 
      sfc <- Cairo.createImageSurface Cairo.FormatARGB32 (floor w) (floor h)
      Cairo.renderWith sfc $ do 
        renderRLayer_InBBox Nothing lyr
      return (set gbuffer (LyBuf (Just sfc)) lyr) 
      
-- | 
updatePageBuf :: RPage -> IO RPage 
updatePageBuf pg = do 
  let dim = view gdimension pg
      mbbox = Just . dimToBBox $ dim 
  nlyrs <- mapM (updateLayerBuf dim mbbox) . view glayers $ pg 
  return (set glayers nlyrs pg)

-- | 
updateHoodleBuf :: RHoodle -> IO RHoodle 
updateHoodleBuf hdl = do 
  let pgs = view gpages hdl 
  npgs <- mapM updatePageBuf pgs
  return . set gpages npgs $ hdl

-------
-- smart constructor for R hoodle structures
-------

-- |
cnstrctRHoodle :: Hoodle -> IO RHoodle
cnstrctRHoodle hdl = do 
  let hid = view hoodleID hdl 
      ttl = view title hdl 
      revs = view revisions hdl 
      pgs = view pages hdl
      embeddedsrc = view embeddedPdf hdl 
  mdoc <- maybe (return Nothing) (\src -> liftIO $ popplerGetDocFromDataURI src)
            embeddedsrc
  npgs <- evalStateT (mapM cnstrctRPage_StateT pgs) (Just (Context "" "" Nothing mdoc)) 
  return $ GHoodle hid ttl revs embeddedsrc (fromList npgs)          
   

-- |
cnstrctRPage_StateT :: Page -> StateT (Maybe Context) IO RPage
cnstrctRPage_StateT pg = do  
  let bkg = view background pg
      dim = view dimension pg 
      lyrs = view layers pg
  nlyrs_lst <- liftIO $ mapM cnstrctRLayer lyrs
  let nlyrs_nonemptylst = if null nlyrs_lst then (emptyRLayer,[]) else (head nlyrs_lst,tail nlyrs_lst) 
      nlyrs = fromNonEmptyList nlyrs_nonemptylst 
  nbkg <- cnstrctRBkg_StateT dim bkg
  return $ GPage dim nbkg nlyrs 
    
-- |
cnstrctRLayer :: Layer -> IO RLayer 
cnstrctRLayer lyr = do 
  nitms <- (mapM cnstrctRItem . view items) lyr 
  return (set gitems nitms emptyRLayer)
