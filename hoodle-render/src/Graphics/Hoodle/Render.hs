{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
-- * xform 
  Xform4Page(..) 
-- * simple rendering using non-R-structure   
, renderStrk
, renderImg
, renderBkg
, renderItem 
, renderLayer
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
-- * some simple render with state
, renderPage_StateT
, initRenderContext
) where

import           Control.Applicative
-- import           Control.Concurrent (putMVar)
import           Control.Concurrent.STM
import           Control.Lens (view,set)
import           Control.Monad.Identity (runIdentity)
import           Control.Monad.State hiding (mapM,mapM_)
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Char8 as C
import           Data.Foldable
import qualified Data.HashMap.Strict as HM
-- import qualified Data.Sequence as Seq (null)
import           Data.Traversable (mapM,sequenceA)
-- import           Data.UUID.V4
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Cairo.SVG as RSVG
import           System.Directory (doesFileExist)
import           System.FilePath (takeExtension)
-- from hoodle-platform 
import           Data.Hoodle.Generic
import           Data.Hoodle.Simple
import           Data.Hoodle.BBox
import           Data.Hoodle.Predefined
import           Data.Hoodle.Zipper
-- from this package
import           Graphics.Hoodle.Render.Background
-- import Graphics.Hoodle.Render.Highlight
import           Graphics.Hoodle.Render.Item
import           Graphics.Hoodle.Render.Primitive
import           Graphics.Hoodle.Render.Type
import           Graphics.Hoodle.Render.Type.HitTest
import           Graphics.Hoodle.Render.Util
import           Graphics.Hoodle.Render.Util.HitTest
-- 
import           Prelude hiding (curry,uncurry,mapM,mapM_,concatMap)

data Xform4Page = Xform4Page { transx :: Double
                             , transy :: Double 
                             , scalex :: Double
                             , scaley :: Double } 
                deriving (Show)

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
renderImg img@(Image src (x,y) (Dim w h))  = do  
    let (x2,y2) = (x+w,y+h)
        -- imgbbx = BBox (x,y) (x2,y2)
        embed = getByteStringIfEmbeddedPNG src 
    msfc <- liftIO $ case embed of         
      Just bstr -> do 
        sfc <- saveTempPNGToCreateSurface bstr 
        return (Just sfc)
      Nothing -> do
	let filesrc = C.unpack (img_src img)
	    filesrcext = takeExtension filesrc 
	    imgaction 
	      | filesrcext == ".PNG" || filesrcext == ".png" = do 
		  b <- doesFileExist filesrc 
		  if b then Just <$> Cairo.imageSurfaceCreateFromPNG filesrc
		       else return Nothing 
	      | filesrcext == ".JPG" || filesrcext == ".jpg" = do 
		  b <- doesFileExist filesrc 
		  if b then Just <$> getJPGandCreateSurface filesrc 
		       else return Nothing 
	      | otherwise = return Nothing 
	imgaction
    case msfc of 
      Nothing -> do -- fall back
        Cairo.setSourceRGBA 0 0 0 1
        Cairo.setLineWidth 10
        Cairo.rectangle x y w h
        Cairo.stroke 
      Just sfc -> do 
        ix <- liftM fromIntegral (Cairo.imageSurfaceGetWidth sfc)
        iy <- liftM fromIntegral (Cairo.imageSurfaceGetHeight sfc)
        Cairo.save 
        Cairo.translate x y 
        Cairo.scale ((x2-x)/ix) ((y2-y)/iy)
        Cairo.setSourceSurface sfc 0 0 
        Cairo.paint 
        Cairo.restore




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
renderLink lnk = 
    let bstr = link_render lnk 
    in if C.null bstr 
       then do
         let lnkbbx = runIdentity (makeBBoxed lnk)
             bbox@(BBox (x0,y0) (x1,y1)) = getBBox lnkbbx
         clipBBox (Just bbox)
         Cairo.setSourceRGBA 0 1 0 1 
         Cairo.rectangle x0 y0 (x1-x0) (y1-y0)
         Cairo.fill
         Cairo.resetClip
         return ()
       else do
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
renderAnchor anc = 
    let bstr = anchor_render anc
    in if C.null bstr 
       then do
         let ancbbx = runIdentity (makeBBoxed anc)
             bbox@(BBox (x0,y0) (x1,y1)) = getBBox ancbbx
         clipBBox (Just bbox)
         Cairo.setSourceRGBA 1 0 0 1 
         Cairo.rectangle x0 y0 (x1-x0) (y1-y0)
         Cairo.fill
         Cairo.resetClip
         return ()
       else do
         let str = C.unpack bstr 
         RSVG.withSvgFromString str $ \rsvg -> do 
           let ancbbx = runIdentity (makeBBoxed anc)
           let (x,y) = (anchor_pos . bbxed_content) ancbbx
               BBox (x1,y1) (x2,y2) = getBBox ancbbx
               (ix',iy') = RSVG.svgGetSize rsvg
               ix = fromIntegral ix' 
               iy = fromIntegral iy'
           clipBBox (Just (getBBox ancbbx))
           Cairo.save 
           Cairo.translate x y 
           Cairo.scale ((x2-x1)/ix) ((y2-y1)/iy)
           RSVG.svgRender rsvg 
           Cairo.restore
           Cairo.resetClip 
           return () 


-- | render item 
renderItem :: Item -> Cairo.Render () 
renderItem (ItemStroke strk) = renderStrk strk
renderItem (ItemImage img) = renderImg img
renderItem (ItemSVG svg) = renderSVG svg
renderItem (ItemLink lnk) = renderLink lnk
renderItem (ItemAnchor anc) = renderAnchor anc 

-- | renderLayer
renderLayer :: Layer -> Cairo.Render ()
renderLayer = mapM_ renderItem . view items


-- |
renderPage :: Page -> Cairo.Render ()
renderPage page = do 
  renderBkg (view background page,view dimension page)
  Cairo.setLineCap Cairo.LineCapRound
  Cairo.setLineJoin Cairo.LineJoinRound
  mapM_ renderLayer . view layers $ page
  Cairo.stroke


-----
-- R-structure 
----

drawFallBackBkg :: Dimension -> Cairo.Render () 
drawFallBackBkg (Dim w h) = do 
  Cairo.setSourceRGBA 1 1 1 1 
  Cairo.rectangle 0 0 w h 
  Cairo.fill 
  

-- | 
renderRBkg :: RenderCache 
           -> (RBackground,Dimension, Maybe Xform4Page) 
           -> Cairo.Render (RBackground,Dimension, Maybe Xform4Page)
renderRBkg = renderRBkg_Buf

-- cache (r,dim,mx) = 
    -- case r of 
    --   (RBkgSmpl _ _ _)     -> renderRBkg_Buf cache (r,dim,mx)
    --      --  renderBkg (rbkg2Bkg r,dim) >> return (r,dim,mx)
    --   (RBkgPDF _ _ _ _ _)  -> renderRBkg_Buf cache (r,dim,mx)
    --   (RBkgEmbedPDF _ _ _) -> renderRBkg_Buf cache (r,dim,mx)

-- |
renderRItem :: RenderCache -> RItem -> Cairo.Render RItem  
renderRItem _ itm@(RItemStroke strk) = renderStrk (bbxed_content strk) >> return itm
renderRItem _cache itm@(RItemImage img msfc) = do
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
renderRItem _ itm@(RItemSVG svgbbx mrsvg) = do 
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
renderRItem _ itm@(RItemLink lnkbbx mrsvg) = do 
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
renderRItem _ itm@(RItemAnchor ancbbx mrsvg) = do
    case mrsvg of
      Nothing -> renderAnchor (bbxed_content ancbbx)
      Just rsvg -> do 
	let (x,y) = (anchor_pos . bbxed_content) ancbbx
	    BBox (x1,y1) (x2,y2) = getBBox ancbbx
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


------------
-- InBBox --
------------

-- | background drawing in bbox 
renderRBkg_InBBox :: RenderCache 
                  -> Maybe BBox 
                  -> (RBackground,Dimension,Maybe Xform4Page) 
                  -> Cairo.Render (RBackground,Dimension, Maybe Xform4Page)
renderRBkg_InBBox cache mbbox (b,dim,mx) = do 
    clipBBox (fmap (flip inflate 1) mbbox)
    renderRBkg_Buf cache (b,dim,mx)
    Cairo.resetClip
    return (b,dim,mx)


-- | render RLayer within BBox after hittest items
renderRLayer_InBBox :: RenderCache -> Maybe BBox -> RLayer -> Cairo.Render RLayer
renderRLayer_InBBox cache mbbox layer = do  
  clipBBox (fmap (flip inflate 2) mbbox)  -- temporary
  let hittestbbox = case mbbox of 
        Nothing -> NotHitted [] 
                   :- Hitted (view gitems layer) 
                   :- Empty 
        Just bbox -> (hltHittedByBBox bbox . view gitems) layer
  (mapM_ (renderRItem cache) . concatMap unHitted  . getB) hittestbbox
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
        (mapM_ (renderRItem cache) . concatMap unHitted  . getB) hittestbbox
        Cairo.resetClip 
        return layer 
    _ -> return layer 

-----------------------
-- draw using buffer -- 
-----------------------

-- | Background rendering using buffer
renderRBkg_Buf :: RenderCache
               -> (RBackground,Dimension,Maybe Xform4Page) 
               -> Cairo.Render (RBackground,Dimension,Maybe Xform4Page)
renderRBkg_Buf cache (b,dim,mx) = do 
    case HM.lookup (rbkg_surfaceid b) cache of
      Nothing -> drawFallBackBkg dim >> return ()
      Just (s,sfc) -> do 
        Cairo.save
        case mx of 
          Nothing -> Cairo.scale (1/s) (1/s) 
          Just xform -> if (scalex xform /s > 0.999 && scalex xform /s < 1.001) 
                          then do Cairo.identityMatrix
                                  Cairo.translate (transx xform) (transy xform)
                                  Cairo.setAntialias Cairo.AntialiasNone
                          else Cairo.scale (1/s) (1/s)
        Cairo.setSourceSurface sfc 0 0 
        Cairo.paint 
        Cairo.restore
    return (b,dim,mx)

-- | 
renderRLayer_InBBoxBuf :: RenderCache 
                       -> Maybe BBox -> RLayer -> Cairo.Render RLayer 
renderRLayer_InBBoxBuf cache mbbox lyr = do
    case view gbuffer lyr of 
      LyBuf (Just sfc) -> do 
        clipBBox mbbox
        Cairo.setSourceSurface sfc 0 0 
        Cairo.paint 
        Cairo.resetClip 
        return lyr 
      _ -> do 
        renderRLayer_InBBox cache mbbox lyr         

-------------------
-- update buffer
-------------------

-- | 
updateLayerBuf :: RenderCache -> Dimension -> Maybe BBox -> RLayer -> IO RLayer
updateLayerBuf cache (Dim w h) mbbox lyr = do 
  case view gbuffer lyr of 
    LyBuf (Just sfc) -> do 
      Cairo.renderWith sfc $ do 
        renderRLayer_InBBox cache mbbox lyr 
      return lyr
    LyBuf Nothing -> do 
      sfc <- Cairo.createImageSurface Cairo.FormatARGB32 (floor w) (floor h)
      Cairo.renderWith sfc $ do 
        renderRLayer_InBBox cache Nothing lyr
      return (set gbuffer (LyBuf (Just sfc)) lyr) 
      
-- | 
updatePageBuf :: RenderCache -> RPage -> IO RPage 
updatePageBuf cache pg = do 
  let dim = view gdimension pg
      mbbox = Just . dimToBBox $ dim 
  nlyrs <- mapM (updateLayerBuf cache dim mbbox) . view glayers $ pg 
  return (set glayers nlyrs pg)

-- | 
updateHoodleBuf :: RenderCache -> RHoodle -> IO RHoodle 
updateHoodleBuf cache hdl = do 
  let pgs = view gpages hdl 
  npgs <- mapM (updatePageBuf cache) pgs
  return . set gpages npgs $ hdl

-------
-- smart constructor for R hoodle structures
-------
 
-- |
cnstrctRHoodle :: Hoodle -> Renderer RHoodle
cnstrctRHoodle hdl = do 
  let hid = view hoodleID hdl 
      ttl = view title hdl 
      revs = view revisions hdl 
      pgs = view pages hdl
      pdf = view embeddedPdf hdl 
      txt = view embeddedText hdl
  (_,(qpdf,qgen)) <- ask
  mdoc <- maybe (return Nothing) (\src -> liftIO $ do
            cmdid <- issuePDFCommandID
            docvar <- atomically newEmptyTMVar
            atomically $ sendPDFCommand qpdf cmdid (GetDocFromDataURI src docvar)
            atomically $ takeTMVar docvar 
          ) pdf
  let getNumPgs doc = liftIO $ do
        cmdid <- issuePDFCommandID
        nvar <- atomically newEmptyTMVar
        atomically $ sendPDFCommand qpdf cmdid (GetNPages doc nvar)
        atomically $ takeTMVar nvar
  mnumpdfpgs <- sequenceA (getNumPgs <$> mdoc)
  -- liftIO $print mnumpdfpgs
  npgs <- evalStateT (mapM cnstrctRPage_StateT pgs) 
                     (Just (Context "" "" Nothing mdoc)) 
  return $ GHoodle hid ttl revs (PDFData <$> pdf <*> mnumpdfpgs) txt (fromList npgs)          
   
-- |
cnstrctRPage_StateT :: Page -> StateT (Maybe Context) Renderer RPage
cnstrctRPage_StateT pg = do  
  let bkg = view background pg
      dim = view dimension pg 
      lyrs = view layers pg
  nlyrs_lst <- lift (mapM cnstrctRLayer lyrs)
  let nlyrs_nonemptylst = if null nlyrs_lst then (emptyRLayer,[]) else (head nlyrs_lst,tail nlyrs_lst) 
      nlyrs = fromNonEmptyList nlyrs_nonemptylst 
  nbkg <- cnstrctRBkg_StateT dim bkg
  return $ GPage dim nbkg nlyrs 
    
-- |
cnstrctRLayer :: Layer -> Renderer RLayer 
cnstrctRLayer lyr = do 
  nitms <- (mapM cnstrctRItem . view items) lyr 
  return (set gitems nitms emptyRLayer)



-------------------------------------------------------
-- simple rendering with pdf (or global information) --
-------------------------------------------------------

-- |
renderPage_StateT :: Page -> StateT Context Cairo.Render ()
renderPage_StateT pg = do  
  let bkg = view background pg
      dim = view dimension pg 
      lyrs = view layers pg
  renderBackground_StateT dim bkg
  lift (mapM_ renderLayer lyrs)
  
-- | 
initRenderContext :: Hoodle -> IO Context
initRenderContext hdl = do
  let pdf = view embeddedPdf hdl 
  mdoc <- join <$> mapM popplerGetDocFromDataURI pdf
  return (Context "" "" Nothing mdoc)


