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
-- * simple rendering using non-R-structure   
  renderStrk
, renderImg
, renderBkg
, renderItem 
, renderPage
-- * render in bbox using non R-structure 
, renderBkg_InBBox
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

import           Control.Lens (view,set,over)
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
import qualified Graphics.UI.Gtk.Poppler.Page as PopplerPage
#endif
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

-- | render image 
renderImg :: Image -> Render () 
renderImg (Image _ (x,y) (Dim w h)) = do  
      setSourceRGBA 0 0 0 1
      setLineWidth 10
      rectangle x y w h
      stroke

-- | render item 
renderItem :: Item -> Render () 
renderItem (ItemStroke strk) = renderStrk strk
renderItem (ItemImage img) = renderImg img

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
  renderBkg (view background page,view dimension page)
  setLineCap LineCapRound
  setLineJoin LineJoinRound
  mapM_ (mapM renderItem . view items) . view layers $ page
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
renderRBkg :: (RBackground,Dimension) -> Render (RBackground,Dimension)
renderRBkg (r@(RBkgSmpl _ _ _),dim) = renderBkg (rbkg2Bkg r,dim) >> return (r,dim)
renderRBkg (r@(RBkgPDF _ _ _ p _),dim) = do
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

-- |
renderRItem :: RItem -> Render RItem  
renderRItem itm@(RItemStroke strk) = renderStrk (strkbbx_strk strk) >> return itm
renderRItem itm@(RItemImage img msfc) = do  
  case msfc of
    Nothing -> renderImg (imgbbx_img img)
    Just sfc -> do 
      let (x,y) = (img_pos . imgbbx_img) img
          BBox (x1,y1) (x2,y2) = getBBox img
      ix <- liftM fromIntegral (imageSurfaceGetWidth sfc)
      iy <- liftM fromIntegral (imageSurfaceGetHeight sfc)
      clipBBox (Just (getBBox img))
      save 
      translate x y 
      scale ((x2-x1)/ix) ((y2-y1)/iy)
      setSourceSurface sfc 0 0 
      paint 
      restore
      resetClip 
  return itm 



------------
-- InBBox --
------------

-- | background drawing in bbox 
renderRBkg_InBBox :: Maybe BBox 
                  -> (RBackground,Dimension) 
                  -> Render (RBackground,Dimension)
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
    return (b,dim)


-- | render RLayer within BBox after hittest items
renderRLayer_InBBox :: Maybe BBox -> RLayer -> Render RLayer
renderRLayer_InBBox mbbox layer = do  
  clipBBox mbbox 
  let hittestbbox = case mbbox of 
        Nothing -> NotHitted [] 
                   :- Hitted (view gitems layer) 
                   :- Empty 
        Just bbox -> (hltHittedByBBox bbox . view gitems) layer
  (mapM_ renderRItem . concatMap unHitted  . getB) hittestbbox
  resetClip
  return layer 


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
      RBkgPDF _ _ _ _ msfc -> do 
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
      LyBuf (Just sfc) -> do clipBBox mbbox
                             setSourceSurface sfc 0 0 
                             paint 
                             resetClip 
      _ -> renderRLayer_InBBox mbbox lyr >> return () 
    return lyr 

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
    

-- |
cnstrctRPage_StateT :: Page -> StateT (Maybe Context) IO RPage
cnstrctRPage_StateT pg = do  
  let bkg = view background pg
      dim = view dimension pg 
      lyrs = view layers pg
  nlyrs <- liftIO $ (liftM fromList . mapM cnstrctRLayer) lyrs 
  nbkg <- cnstrctRBkg_StateT dim bkg
  return . set glayers nlyrs $ emptyGPage dim nbkg 
    

cnstrctRLayer :: Layer -> IO RLayer 
cnstrctRLayer lyr = do 
  nitms <- (mapM cnstrctRItem . view items) lyr 
  return (set gitems nitms emptyRLayer)

                 
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
          renderWith sfc $ renderBkg (bkg,dim) 
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

