{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
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
-- * dummy rendering 
  renderRBkg_Dummy  
-- * simple rendering using non-R-structure   
, renderStrk
, renderImg
, renderBkg
, renderPage
-- * render in bbox using non R-structure 
, renderBkg_InBBox

-- * simple rendering using R-structure 
, renderRBkg

-- * nopdf
, renderRBkg_NoPDF

-- * render in bbox
, renderRLayer_InBBox
, renderRBkg_InBBox 

-- * render only bbox (for debug purpose)
, renderStrkBBx_BBoxOnly
, renderImgBBx_BBoxOnly
, renderRLayer_BBoxOnly
, renderRPage_BBoxOnly

-- * render using buf 
, renderRBkg_Buf
, renderRLayer_InBBoxBuf

-- * buffer update 
, updateLayerBuf
, updatePageBuf 
, updateHoodleBuf 
-- * construct R-structure from non-R-structure 
, mkRLayer
, cnstrctRBkg_StateT
, cnstrctRPage_StateT
, cnstrctRHoodle  
  
) where

import           Control.Lens 
import           Control.Monad.State hiding (mapM,mapM_)
import           Data.Foldable
import           Data.Traversable (mapM)
import qualified Data.Map as M
import           Data.Monoid
import           Graphics.Rendering.Cairo
-- from hoodle-platform 
import Data.Hoodle.Generic
import Data.Hoodle.Simple
import Data.Hoodle.BBox
import Data.Hoodle.Predefined 
#ifdef POPPLER
import qualified Graphics.UI.Gtk.Poppler.Document as Poppler
import qualified Graphics.UI.Gtk.Poppler.Page as PopplerPage
#endif
-- from this package
-- import Graphics.Hoodle.Render.Simple 
import Graphics.Hoodle.Render.Background 
import Graphics.Hoodle.Render.Primitive
import Graphics.Hoodle.Render.Type 
import Graphics.Hoodle.Render.Type.Background
import Graphics.Hoodle.Render.Type.HitTest
import Graphics.Hoodle.Render.Util 
import Graphics.Hoodle.Render.Util.HitTest 
-- 
import Prelude hiding (curry,uncurry,mapM,mapM_,concatMap)


----- 
-- Dummy (for testing) 
-----

renderRBkg_Dummy :: (RBackground,Dimension) -> Render () 
renderRBkg_Dummy (_,Dim w h) = do 
    setSourceRGBA 1 1 1 1
    rectangle 0 0 w h 
    fill 

-----
-- simple 
--- 

-- | render stroke 
renderStrk :: Stroke -> Render ()
renderStrk s@(Stroke _ _ w d) = do 
    let opacity = if stroke_tool s == "highlighter" 
                  then predefined_highlighter_opacity
                  else 1.0
    case M.lookup (stroke_color s) predefined_pencolor of
      Just (r,g,b,a) -> setSourceRGBA r g b (a*opacity) 
      Nothing -> setSourceRGBA 0 0 0 1
    setLineWidth w
    setLineCap LineCapRound
    setLineJoin LineJoinRound
    drawStrokeCurve d
    stroke
renderStrk s@(VWStroke _ _ d) = do 
    let opacity = if stroke_tool s == "highlighter" 
                  then predefined_highlighter_opacity
                  else 1.0
    case M.lookup (stroke_color s) predefined_pencolor of
      Just (r,g,b,a) -> setSourceRGBA r g b (a*opacity) 
      Nothing -> setSourceRGBA 0 0 0 1
    setFillRule FillRuleWinding
    drawVWStrokeCurve d
    fill 

-- | render image 
renderImg :: Image -> Render () 
renderImg (Image _ (x,y) (Dim w h)) = do  
      setSourceRGBA 0 0 0 1
      setLineWidth 10
      rectangle x y w h
      stroke

{-
-- | render item 
renderItm :: Item -> Render () 
renderItm (ItemStroke strk) = renderStrk strk
renderItm (ItemImage img) = renderImg img
-}


-- | render background without any constraint 
renderBkg :: (Background,Dimension) -> Render () 
renderBkg (Background _typ col sty,Dim w h) = do 
    let c = M.lookup col predefined_bkgcolor  
    case c of 
      Just (r,g,b,_a) -> setSourceRGB r g b 
      Nothing        -> setSourceRGB 1 1 1 
    rectangle 0 0 w h 
    fill
    drawRuling w h sty
renderBkg (BackgroundPdf _ _ _ _,Dim w h) = do 
    setSourceRGBA 1 1 1 1
    rectangle 0 0 w h 
    fill


-- |
renderPage :: Page -> Render ()
renderPage page = do 
  -- let itms = (view items . (!!0) . view layers) page 
  -- let lyrs = view layers page 
  renderBkg (view background page,view dimension page)
  setLineCap LineCapRound
  setLineJoin LineJoinRound
  -- mapM_ renderItm  itms
  mapM_ (mapM renderStrk . view strokes) . view layers $ page
  stroke

--- 
-- non-R but in bbox 
--- 


-- | render Background in BBox 
renderBkg_InBBox :: Maybe BBox -> Dimension -> Background -> Render ()
renderBkg_InBBox mbbox dim@(Dim w h) (Background typ col sty) = do 
    let mbbox2 = toMaybe $ fromMaybe mbbox `mappend` (Intersect (Middle (dimToBBox dim)))
    case mbbox2 of 
      Nothing -> renderBkg (Background typ col sty,Dim w h)
      Just bbox@(BBox (x1,y1) (x2,y2)) -> do 
        let c = M.lookup col predefined_bkgcolor  
        case c of 
          Just (r,g,b,_a) -> setSourceRGB r g b 
          Nothing        -> setSourceRGB 1 1 1 
        rectangle x1 y1 (x2-x1) (y2-y1)
        fill
        drawRuling_InBBox bbox w h sty
renderBkg_InBBox _ _  (BackgroundPdf _ _ _ _) = 
    error "BackgroundPdf in renderBkg_InBBox"

-----
-- R-structure 
----

-- | 
renderRBkg :: (RBackground,Dimension) -> Render ()
renderRBkg r@(RBkgSmpl _ _ _,dim) = renderBkg (rbkg2Bkg (fst r),dim)
renderRBkg (RBkgPDF _ _ _ p _,dim) = do
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

--------------
-- BBoxOnly --
--------------

-- | render only bounding box of a StrokeBBox      
renderStrkBBx_BBoxOnly :: StrokeBBox -> Render () 
renderStrkBBx_BBoxOnly sbbox = do  
    let s = strkbbx_strk sbbox
    case M.lookup (stroke_color s) predefined_pencolor of 
      Just (r,g,b,a) -> setSourceRGBA r g b a
      Nothing -> setSourceRGBA 0 0 0 1 
    setSourceRGBA 0 0 0 1
    setLineWidth (stroke_width s) 
    let BBox (x1,y1) (x2,y2) = strkbbx_bbx sbbox
    rectangle x1 y1 (x2-x1) (y2-y1)
    stroke
  
-- |     
renderImgBBx_BBoxOnly :: ImageBBox -> Render () 
renderImgBBx_BBoxOnly ibbox = do 
    setSourceRGBA 0 0 0 1
    setLineWidth 10
    let BBox (x1,y1) (x2,y2) = imgbbx_bbx ibbox
    rectangle x1 y1 (x2-x1) (y2-y1)
    stroke
    
{-
-- | 
renderRItem_BBoxOnly :: RItem -> Render () 
renderRItem_BBoxOnly (RItemStroke sbbox) = renderStrkBBx_BBoxOnly sbbox
renderRItem_BBoxOnly (RItemImage ibbox _) = renderImgBBx_BBoxOnly ibbox
-}

-- | 
renderRLayer_BBoxOnly :: RLayer -> Render ()
renderRLayer_BBoxOnly = mapM_ renderStrkBBx_BBoxOnly . view gstrokes 
                        -- mapM_  renderRItem_BBoxOnly . view gitems




  
-- | render only bounding box of a StrokeBBox      
renderRPage_BBoxOnly :: RPage -> Render ()  
renderRPage_BBoxOnly page = do
    let dim = view gdimension page
        bkg = view gbackground page 
        lyrs =  view glayers page
    -- cairoDrawBackground (toPage id page)
    renderRBkg_NoPDF (bkg,dim)
    mapM_ renderRLayer_BBoxOnly lyrs

-----------
-- NoPDF -- 
-----------

-- | render background without pdf 
renderRBkg_NoPDF :: (RBackground,Dimension) -> Render ()
renderRBkg_NoPDF r@(RBkgSmpl _ _ _,_) = renderRBkg r
renderRBkg_NoPDF (RBkgPDF _ _ _ _ _,_) = return ()


------------
-- InBBox --
------------

-- | background drawing in bbox 
renderRBkg_InBBox :: Maybe BBox -> (RBackground,Dimension) -> Render ()
renderRBkg_InBBox mbbox (b,dim) = do 
    case b of 
      RBkgSmpl _ _ _ -> do 
        clipBBox mbbox
        renderRBkg_Buf (b,dim)
        resetClip
      RBkgPDF _ _ _ _ _ -> do 
        clipBBox mbbox
        renderRBkg_Buf (b,dim)
        resetClip


-- | render RLayer within BBox after hittest items
renderRLayer_InBBox :: Maybe BBox -> RLayer -> Render () 
renderRLayer_InBBox mbbox layer = do  
  clipBBox mbbox 
  let hittestbbox = case mbbox of 
        Nothing -> NotHitted [] 
                   :- Hitted (view gstrokes layer) 
                   :- Empty 
        Just bbox -> (hltStrksHittedByBBox bbox . view gstrokes) layer
  (mapM_ (renderStrk.strkbbx_strk) . concatMap unHitted  . getB) hittestbbox
  resetClip


-----------------------
-- draw using buffer -- 
-----------------------

-- | Background rendering using buffer
renderRBkg_Buf :: (RBackground,Dimension) -> Render ()
renderRBkg_Buf (b,dim) = do 
    case b of 
      RBkgSmpl _ _ msfc  -> do  
        case msfc of 
          Nothing -> renderRBkg (b,dim)
          Just sfc -> do 
            setSourceSurface sfc 0 0 
            -- setOperator OperatorSource
            -- setAntialias AntialiasNone
            paint 
      RBkgPDF _ _ _ _ msfc -> do 
        case msfc of 
          Nothing -> renderRBkg (b,dim)
          Just sfc -> do 
            setSourceSurface sfc 0 0 
            -- setOperator OperatorSource
            -- setAntialias AntialiasNone
            paint 

-- | 
renderRLayer_InBBoxBuf :: Maybe BBox -> RLayer -> Render ()
renderRLayer_InBBoxBuf mbbox lyr = do
  case view gbuffer lyr of 
    LyBuf (Just sfc) -> do clipBBox mbbox
                           setSourceSurface sfc 0 0 
                           -- setOperator OperatorSource
                           -- setAntialias AntialiasNone
                           paint 
                           resetClip 
    _ -> renderRLayer_InBBox mbbox lyr 


-------------------
-- update buffer
-------------------

-- | 
updateLayerBuf :: Maybe BBox -> RLayer -> IO RLayer
updateLayerBuf mbbox lyr = do 
  case view gbuffer lyr of 
    LyBuf (Just sfc) -> do 
      renderWith sfc $ do 
        clearBBox mbbox        
        renderRLayer_InBBox mbbox lyr 
      return lyr
    _ -> return lyr


-- | 
updatePageBuf :: RPage -> IO RPage 
updatePageBuf pg = do 
  let dim = view gdimension pg
      mbbox = Just . dimToBBox $ dim 
  nlyrs <- mapM (updateLayerBuf mbbox) . view glayers $ pg 
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
  let ttl = view title hdl 
      pgs = view pages hdl
  npgs <- evalStateT (mapM cnstrctRPage_StateT pgs) Nothing 
  return . set gtitle ttl . set gpages (fromList npgs) $ emptyGHoodle 
    
{-  
-- |
mkAllTPageBBoxMapPDF :: [Page] -> IO [TPageBBoxMapPDF]
mkAllTPageBBoxMapPDF pgs = evalStateT (mapM mkPagePDF pgs) Nothing 
-}

-- |
cnstrctRPage_StateT :: Page -> StateT (Maybe Context) IO RPage
cnstrctRPage_StateT pg = do  
  let bkg = view background pg
      dim = view dimension pg 
      lyrs = view layers pg
      nlyrs = fromList . fmap mkRLayer $ lyrs 
  nbkg <- cnstrctRBkg_StateT dim bkg
  return . set glayers nlyrs $ emptyGPage dim nbkg 
    
-- GPage dim nbkg (gFromList . Prelude.map fromLayer $ ls)
  
mkRLayer :: Layer -> RLayer   
mkRLayer lyr = let nstrks = map mkStrokeBBox . view strokes $ lyr 
               in set gstrokes nstrks emptyRLayer
  
  
-- |
cnstrctRBkg_StateT :: Dimension -> Background 
                   -> StateT (Maybe Context) IO RBackground
cnstrctRBkg_StateT dim@(Dim w h) bkg = do  
  let rbkg = bkg2RBkg bkg
  case rbkg of 
    RBkgSmpl _c _s msfc -> do 
      case msfc of 
        Just _ -> return rbkg
        Nothing -> do 
          sfc <- liftIO $ createImageSurface FormatARGB32 (floor w) (floor h)
          renderWith sfc $ do 
            -- cairoDrawBkg dim bkg
            renderBkg (bkg,dim) 
          return rbkg { rbkg_cairosurface = Just sfc}
    RBkgPDF md mf pn _ _ -> do 
#ifdef POPPLER
      mctxt <- get 
      case mctxt of
        Nothing -> do 
          case (md,mf) of 
            (Just d, Just f) -> do 
              mdoc <- liftIO $ popplerGetDocFromFile f
              put $ Just (Context d f mdoc)
              case mdoc of 
                Just doc -> do  
                  (mpg,msfc) <- liftIO $ popplerGetPageFromDoc doc pn 
                  return (rbkg {rbkg_popplerpage = mpg, rbkg_cairosurface = msfc})
                Nothing -> error "no pdf doc? in mkBkgPDF"
            _ -> return rbkg 
        Just (Context _oldd _oldf olddoc) -> do 
          (mpage,msfc) <- case olddoc of 
            Just doc -> do 
              liftIO $ popplerGetPageFromDoc doc pn
            Nothing -> return (Nothing,Nothing) 
          return $ RBkgPDF md mf pn mpage msfc
#else
          return rbkg
#endif  

{-
-- | 
mkTLayerBBoxBufFromNoBuf :: Dimension -> TLayerBBox -> IO (TLayerBBoxBuf LyBuf)
mkTLayerBBoxBufFromNoBuf (Dim w h) lyr = do 
  let strs = view g_strokes lyr 
  sfc <- createImageSurface FormatARGB32 (floor w) (floor h)
  renderWith sfc (cairoDrawLayerBBox (Just (BBox (0,0) (w,h))) lyr)
  return $ GLayerBuf { gbuffer = LyBuf (Just sfc), 
                       gbstrokes = strs }  -- temporary

    
-- | 
mkTPageBBoxMapPDFBufFromNoBuf :: TPageBBoxMapPDF -> IO TPageBBoxMapPDFBuf
mkTPageBBoxMapPDFBufFromNoBuf page = do 
  let dim = view g_dimension page
      bkg = view g_background page
      ls =  view g_layers page
  ls' <- mapM (mkTLayerBBoxBufFromNoBuf dim) ls
  return . GPage dim bkg . gFromList . gToList $ ls'
      
-- | 
mkTHoodleBBoxMapPDFBufFromNoBuf :: THoodleBBoxMapPDF 
                                    -> IO THoodleBBoxMapPDFBuf
mkTHoodleBBoxMapPDFBufFromNoBuf hdl = do 
  let title = view g_title hdl
      pages = view g_pages hdl 
  pages' <- mapM mkTPageBBoxMapPDFBufFromNoBuf pages
 
  return $ GHoodle title pages'

-}

{-
cairoOneStrokeSelected :: StrokeBBox -> Render ()
cairoOneStrokeSelected sbbox = do 
  let s = gToStroke sbbox 
  case s of     
    Img _ _ _ -> cairoOneStrokeBBoxOnly sbbox 
    _ -> do     
      case M.lookup (stroke_color s) predefined_pencolor of 
        Just (r,g,b,a) -> setSourceRGBA r g b a
        Nothing -> setSourceRGBA 0 0 0 1 
      case s of
        Stroke _ _ w d -> do  
          setLineWidth (w * 4.0) 
          setLineCap LineCapRound
          setLineJoin LineJoinRound
          drawOneStrokeCurve d
          stroke
          setSourceRGBA 1 1 1 1
          setLineWidth w
          drawOneStrokeCurve . stroke_data $ s 
          stroke
        VWStroke _ _ d -> do  
          setFillRule FillRuleWinding
          drawOneVWStrokeCurve $ map (\(x,y,z)->(x,y,4*z)) d
          fill  
          setSourceRGBA 1 1 1 1
          drawOneVWStrokeCurve d     
          fill
        _ -> error "in cairoOneStrokeSelected"
-}    
    
{-
cairoDrawLayerBBoxOnly :: TLayerBBox -> Render () 
cairoDrawLayerBBoxOnly  = mapM_ cairoOneStrokeBBoxOnly . gstrokes 

----

cairoDrawPageBBox :: Maybe BBox -> TPageBBoxMap -> Render ()
cairoDrawPageBBox mbbox page = do 
    cairoDrawBackgroundBBox mbbox (gdimension page) (gbackground page) 
    mapM_ (cairoDrawLayerBBox mbbox) (glayers page)


cairoDrawLayerBBox :: Maybe BBox -> TLayerBBox -> Render () 
cairoDrawLayerBBox mbbox layer = do  
  clipBBox mbbox 
  let hittestbbox = case mbbox of 
                       Nothing -> NotHitted [] 
                                  :- Hitted (gstrokes layer) 
                                  :- Empty 
                       Just bbox -> mkHitTestBBoxBBox bbox (gstrokes layer)
  mapM_ drawOneStroke . map gToStroke . concatMap unHitted  . getB $ hittestbbox
  resetClip

cairoDrawBackgroundBBox :: Maybe BBox -> Dimension -> Background -> Render ()
cairoDrawBackgroundBBox mbbox dim@(Dim w h) (Background typ col sty) = do 
    let mbbox2 = toMaybe $ fromMaybe mbbox `mappend` (Intersect (Middle (dimToBBox dim)))
    case mbbox2 of 
      Nothing -> cairoDrawBkg (Dim w h) (Background typ col sty)
      Just bbox@(BBox (x1,y1) (x2,y2)) -> do 
        let c = M.lookup col predefined_bkgcolor  
        case c of 
          Just (r,g,b,_a) -> setSourceRGB r g b 
          Nothing        -> setSourceRGB 1 1 1 
        rectangle x1 y1 (x2-x1) (y2-y1)
        fill
        cairoDrawRulingBBox bbox w h sty
cairoDrawBackgroundBBox _ _  (BackgroundPdf _ _ _ _) = 
    error "BackgroundPdf in cairoDrawBackgroundBBox"

-}