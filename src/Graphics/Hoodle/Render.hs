{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
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
-- * render in bbox using non R-structure 
-- , renderBkg_InBBox
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
-- import qualified Data.Map as M
-- import           Data.Monoid
import           Graphics.Rendering.Cairo
-- from hoodle-platform 
import           Data.Hoodle.Generic
import           Data.Hoodle.Simple
import           Data.Hoodle.BBox
import           Data.Hoodle.Predefined 
import           Data.Hoodle.Zipper 
#ifdef POPPLER
import qualified Graphics.UI.Gtk.Poppler.Page as PopplerPage
#endif
import qualified Graphics.Rendering.Cairo.SVG as RSVG
-- from this package
-- import Graphics.Hoodle.Render.Simple 
import Graphics.Hoodle.Render.Background 
import Graphics.Hoodle.Render.Item 
import Graphics.Hoodle.Render.Primitive
import Graphics.Hoodle.Render.Type 
import Graphics.Hoodle.Render.Type.HitTest
import Graphics.Hoodle.Render.Util 
import Graphics.Hoodle.Render.Util.HitTest 
-- 
import Prelude hiding (curry,uncurry,mapM,mapM_,concatMap)



-----
-- simple 
--- 

-- | render stroke 
renderStrk :: Stroke -> Render ()
renderStrk s@(Stroke _ _ w d) = do 
    let opacity = if stroke_tool s == "highlighter" 
                  then predefined_highlighter_opacity
                  else 1.0
    case getPenColor (stroke_color s) of 
      Just (r,g,b,a) -> setSourceRGBA r g b (a*opacity) 
      Nothing -> setSourceRGBA 0.5 0.5 0.5 1
    setLineWidth w
    setLineCap LineCapRound
    setLineJoin LineJoinRound
    drawStrokeCurve d
    stroke 
renderStrk s@(VWStroke _ _ d) = do 
    let opacity = if stroke_tool s == "highlighter" 
                  then predefined_highlighter_opacity
                  else 1.0
    case getPenColor (stroke_color s) of
      Just (r,g,b,a) -> setSourceRGBA r g b (a*opacity) 
      Nothing -> setSourceRGBA 0.5 0.5 0.5 1
    setFillRule FillRuleWinding
    drawVWStrokeCurve d
    fill 

-- | render image : not fully implemented 
renderImg :: Image -> Render () 
renderImg (Image _ (x,y) (Dim w h)) = do  
      setSourceRGBA 0 0 0 1
      setLineWidth 10
      rectangle x y w h
      stroke

-- | render svg  
renderSVG :: SVG -> Render () 
renderSVG svg@(SVG _ _ bstr _ _) = do  
    let str = C.unpack bstr 
    RSVG.withSvgFromString str $ \rsvg -> do 
      let svgbbx = runIdentity (makeBBoxed svg)
      let (x,y) = (svg_pos . bbxed_content) svgbbx
          BBox (x1,y1) (x2,y2) = getBBox svgbbx
          (ix',iy') = RSVG.svgGetSize rsvg
          ix = fromIntegral ix' 
          iy = fromIntegral iy'
      clipBBox (Just (getBBox svgbbx))
      save 
      translate x y 
      scale ((x2-x1)/ix) ((y2-y1)/iy)
      RSVG.svgRender rsvg 
      restore
      resetClip 
      return () 

-- | render svg  
renderLink :: Link -> Render () 
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
      save 
      translate x y 
      scale ((x2-x1)/ix) ((y2-y1)/iy)
      RSVG.svgRender rsvg 
      restore
      resetClip 
      return () 


-- | render item 
renderItem :: Item -> Render () 
renderItem (ItemStroke strk) = renderStrk strk
renderItem (ItemImage img) = renderImg img
renderItem (ItemSVG svg) = renderSVG svg
renderItem (ItemLink lnk) = renderLink lnk

-- |
renderPage :: Page -> Render ()
renderPage page = do 
  renderBkg (view background page,view dimension page)
  setLineCap LineCapRound
  setLineJoin LineJoinRound
  mapM_ (mapM renderItem . view items) . view layers $ page
  stroke


-----
-- R-structure 
----

-- | 
renderRBkg :: (RBackground,Dimension) -> Render (RBackground,Dimension)
renderRBkg (r,dim) = 
    case r of 
      (RBkgSmpl _ _ _) -> 
        drawBkgAndRecord (renderBkg (rbkg2Bkg r,dim))
        -- renderBkg (rbkg2Bkg r,dim) >> return (r,dim)
      (RBkgPDF _ _ _ p _) -> 
        maybe (return (r,dim)) 
              (\pg -> drawBkgAndRecord (bkgPdfRender pg)) p
      (RBkgEmbedPDF _ p _) -> 
        maybe (return (r,dim)) 
              (\pg -> drawBkgAndRecord (bkgPdfRender pg)) p 
  where 
    drawBkgAndRecord rdr = do 
      rdr 
      case rbkg_cairosurface r of
        Nothing -> return ()
        Just sfc -> liftIO $ renderWith sfc rdr
      return (r,dim)
    bkgPdfRender pg = do 
      let Dim w h = dim 
      setSourceRGBA 1 1 1 1
      rectangle 0 0 w h 
      fill
#ifdef POPPLER
      PopplerPage.pageRender pg
#endif

{-
        case p of 
          Nothing -> return () 
          Just pg -> do 
            let Dim w h = dim 
            setSourceRGBA 1 1 1 1
            rectangle 0 0 w h 
            fill
#ifdef POPPLER
            PopplerPage.pageRender pg
#endif     
        return (r,dim) 
-}


{-
  case view gbuffer layer of 
    LyBuf (Just sfc) -> do 
      liftIO $ renderWith sfc $ do 
        -- renderRLayer_InBBox mbbox lyr 
        clipBBox (fmap (flip inflate 1) mbbox )
        setSourceRGBA 0 0 0 0 
        setOperator OperatorSource
        paint
        setOperator OperatorOver
        (mapM_ renderRItem . concatMap unHitted  . getB) hittestbbox
        resetClip 
        return layer 
    _ -> return layer 
-}


-- |
renderRItem :: RItem -> Render RItem  
renderRItem itm@(RItemStroke strk) = renderStrk (bbxed_content strk) >> return itm
renderRItem itm@(RItemImage img msfc) = do  
  case msfc of
    Nothing -> renderImg (bbxed_content img)
    Just sfc -> do 
      let (x,y) = (img_pos . bbxed_content) img
          BBox (x1,y1) (x2,y2) = getBBox img
      ix <- liftM fromIntegral (imageSurfaceGetWidth sfc)
      iy <- liftM fromIntegral (imageSurfaceGetHeight sfc)
      -- clipBBox (Just (getBBox img))
      save 
      translate x y 
      scale ((x2-x1)/ix) ((y2-y1)/iy)
      setSourceSurface sfc 0 0 
      paint 
      restore
      -- resetClip 
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
      clipBBox (Just (getBBox svgbbx))
      save 
      translate x y 
      scale ((x2-x1)/ix) ((y2-y1)/iy)
      RSVG.svgRender rsvg 
      restore
      resetClip 
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
      clipBBox (Just (getBBox lnkbbx))
      save 
      translate x y 
      scale ((x2-x1)/ix) ((y2-y1)/iy)
      RSVG.svgRender rsvg 
      restore
      resetClip 
      return () 
  return itm 



------------
-- InBBox --
------------

-- | background drawing in bbox 
renderRBkg_InBBox :: Maybe BBox 
                  -> (RBackground,Dimension) 
                  -> Render (RBackground,Dimension)
renderRBkg_InBBox mbbox (b,dim) = do 
    clipBBox mbbox
    renderRBkg_Buf (b,dim)
    resetClip
    return (b,dim)

{-    case b of 
      RBkgSmpl _ _ _ -> do 
        clipBBox mbbox
        renderRBkg_Buf (b,dim)
        resetClip
      RBkgPDF _ _ _ _ _ -> do 
        clipBBox mbbox
        renderRBkg_Buf (b,dim)
        resetClip
      RBkgEmbedPDF _ _ _ -> do 
        clipBBox mbbox
        renderRBkg_Buf (b,dim)
        resetClip -}


-- | render RLayer within BBox after hittest items
renderRLayer_InBBox :: Maybe BBox -> RLayer -> Render RLayer
renderRLayer_InBBox mbbox layer = do  
  clipBBox (fmap (flip inflate 1) mbbox) 
  let hittestbbox = case mbbox of 
        Nothing -> NotHitted [] 
                   :- Hitted (view gitems layer) 
                   :- Empty 
        Just bbox -> (hltHittedByBBox bbox . view gitems) layer
  (mapM_ renderRItem . concatMap unHitted  . getB) hittestbbox
  resetClip
  -- return layer 
  -- simply twice rendering if whole redraw happening 
  case view gbuffer layer of 
    LyBuf (Just sfc) -> do 
      liftIO $ renderWith sfc $ do 
        -- renderRLayer_InBBox mbbox lyr 
        clipBBox (fmap (flip inflate 1) mbbox )
        setSourceRGBA 0 0 0 0 
        setOperator OperatorSource
        paint
        setOperator OperatorOver
        (mapM_ renderRItem . concatMap unHitted  . getB) hittestbbox
        resetClip 
        return layer 
    _ -> return layer 
  



-----------------------
-- draw using buffer -- 
-----------------------

-- | Background rendering using buffer
renderRBkg_Buf :: (RBackground,Dimension) 
               -> Render (RBackground,Dimension)
renderRBkg_Buf (b,dim) = do 
    case b of 
      RBkgSmpl _ _ msfc  -> do  
        case msfc of 
          Nothing -> renderRBkg (b,dim) >> return ()
          Just sfc -> do 
            setSourceSurface sfc 0 0 
            paint 
      RBkgPDF _ _ _n _ msfc -> do 
        case msfc of 
          Nothing -> renderRBkg (b,dim) >> return ()
          Just sfc -> do 
            setSourceSurface sfc 0 0 
            paint 
      RBkgEmbedPDF _ _ msfc -> do 
        case msfc of 
          Nothing -> renderRBkg (b,dim) >> return ()
          Just sfc -> do 
            setSourceSurface sfc 0 0 
            paint 

    return (b,dim)

-- | 
renderRLayer_InBBoxBuf :: Maybe BBox -> RLayer -> Render RLayer 
renderRLayer_InBBoxBuf mbbox lyr = do
    case view gbuffer lyr of 
      LyBuf (Just sfc) -> do 
        clipBBox mbbox
        setSourceSurface sfc 0 0 
        paint 
        resetClip 
        return lyr 
      _ -> do 
        renderRLayer_InBBox mbbox lyr         
        -- return lyr 

-------------------
-- update buffer
-------------------

-- | 
updateLayerBuf :: Dimension -> Maybe BBox -> RLayer -> IO RLayer
updateLayerBuf (Dim w h) mbbox lyr = do 
  case view gbuffer lyr of 
    LyBuf (Just sfc) -> do 
      renderWith sfc $ do 
        renderRLayer_InBBox mbbox lyr 
      return lyr
    LyBuf Nothing -> do 
      -- return lyr 
      sfc <- createImageSurface FormatARGB32 (floor w) (floor h)
      renderWith sfc $ do 
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
      pgs = view pages hdl
      embeddedsrc = view embeddedPdf hdl 
#ifdef POPPLER
  mdoc <- maybe (return Nothing) (\src -> liftIO $ popplerGetDocFromDataURI src)
            embeddedsrc
#else 
  let mdoc = Nothing 
#endif 
  npgs <- evalStateT (mapM cnstrctRPage_StateT pgs) (Just (Context "" "" Nothing mdoc)) 
  return $ GHoodle hid ttl embeddedsrc (fromList npgs)          
   
  -- set gtitle ttl . set gembeddedpdf embeddedsrc . set gpages (fromList npgs) <$> emptyGHoodle 
    

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
  return $ GPage dim nbkg nlyrs -- emptyGPage dim nbkg 
    

cnstrctRLayer :: Layer -> IO RLayer 
cnstrctRLayer lyr = do 
  nitms <- (mapM cnstrctRItem . view items) lyr 
  return (set gitems nitms emptyRLayer)

                 
