{-# LANGUAGE OverloadedStrings, TypeFamilies, TypeSynonymInstances, 
             MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, 
             CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.BBoxMapPDF 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Graphics.Hoodle.Render.BBoxMapPDF where

import Control.Applicative
import Data.Foldable 
import Data.IntMap
import Data.Traversable 

import           Control.Category
import           Control.Compose
import           Control.Lens
import           Control.Monad.State hiding (get, mapM_, mapM,sequence)
import qualified Control.Monad.State as St (get)
import           Graphics.Rendering.Cairo
-- from hoodle-platform 
import           Data.Hoodle.BBox
import           Data.Hoodle.Buffer
import           Data.Hoodle.Generic
import           Data.Hoodle.Map
import           Data.Hoodle.Select
import           Data.Hoodle.Simple
-- from this package 
import           Graphics.Hoodle.Render.BBox
import           Graphics.Hoodle.Render.Generic
import           Graphics.Hoodle.Render.PDFBackground
import           Graphics.Hoodle.Render.Simple
import           Graphics.Hoodle.Render.Type 
-- 
import Prelude hiding ((.),id,mapM, mapM_, sequence )

-- | 
type TPageBBoxMapPDF = TPageBBoxMapBkg BackgroundPDFDrawable 

-- | 
type THoodleBBoxMapPDF = THoodleBBoxMapBkg BackgroundPDFDrawable

-- | 
type TTempPageSelectPDF = GPage BackgroundPDFDrawable (TLayerSelectInPage []) TLayerBBox

-- | 
type TTempHoodleSelectPDF = GSelect (IntMap TPageBBoxMapPDF) (Maybe (Int, TTempPageSelectPDF))

-- | 
newtype LyBuf = LyBuf { mbuffer :: Maybe Surface } 

-- | 
type instance StrokeTypeFromLayer (TLayerBBoxBuf b) = StrokeBBox 

-- | 
type TPageBBoxMapPDFBuf = 
  TPageBBoxMapBkgBuf BackgroundPDFDrawable LyBuf
  
-- |   
type THoodleBBoxMapPDFBuf = 
  THoodleBBoxMapBkgBuf BackgroundPDFDrawable LyBuf
  
-- |
type TTempPageSelectPDFBuf = 
  GPage BackgroundPDFDrawable (TLayerSelectInPageBuf ZipperSelect) (TLayerBBoxBuf LyBuf)

-- | 
type TTempHoodleSelectPDFBuf = 
  GSelect (IntMap TPageBBoxMapPDFBuf) (Maybe (Int, TTempPageSelectPDFBuf))

-- | 
instance GCast (TLayerBBox)  (TLayerBBoxBuf LyBuf) where
  gcast lyr = GLayerBuf (LyBuf Nothing) (view g_strokes lyr) 

-- | 
instance GCast Layer (TLayerBBoxBuf LyBuf) where
  gcast lyr = gcast (fromLayer lyr :: TLayerBBox)

-- | 
emptyTLayerBBoxBufLyBuf :: IO (TLayerBBoxBuf LyBuf)
emptyTLayerBBoxBufLyBuf = updateLayerBuf Nothing $ gcast emptyLayer 

-- | 
instance GBackgroundable BackgroundPDFDrawable where 
  gFromBackground = bkgPDFFromBkg 
  gToBackground = bkgFromBkgPDF

-- | 
tlayerBBoxFromTLayerSelect :: TLayerSelect TLayerBBox -> TLayerBBox 
tlayerBBoxFromTLayerSelect l = 
  case unTEitherAlterHitted (gstrokes l) of
    Left strs -> GLayer strs 
    Right alist -> GLayer . Prelude.concat $ interleave id unHitted alist

-- | 
tlayerbufFromTLayerSelectBuf :: TLayerSelectBuf (TLayerBBoxBuf b) 
                                -> (TLayerBBoxBuf b) 
tlayerbufFromTLayerSelectBuf l = 
  case unTEitherAlterHitted (view g_bstrokes l) of
    Left strs -> GLayerBuf (view g_buffer l) strs 
    Right alist -> GLayerBuf (view g_buffer l) . Prelude.concat 
                   $ interleave id unHitted alist
  
-- | 
instance GCast TTempPageSelectPDF TPageBBoxMapPDF where      
  gcast = tpageBBoxMapPDFFromTTempPageSelectPDF

-- |   
instance GCast TTempPageSelectPDFBuf TPageBBoxMapPDFBuf where      
  gcast = tpageBBoxMapPDFBufFromTTempPageSelectPDFBuf
      
-- | 
tpageBBoxMapPDFBufFromTTempPageSelectPDFBuf :: TTempPageSelectPDFBuf 
                                               -> TPageBBoxMapPDFBuf
tpageBBoxMapPDFBufFromTTempPageSelectPDFBuf p = 
  let TLayerSelectInPageBuf s others = view g_layers p 
      s' = tlayerbufFromTLayerSelectBuf s
      normalizedothers = case others of   
        NoSelect [] -> error "something wrong in tpageBBoxMapPDFBufFromTTempPageSelectPDFBuf" 
        NoSelect (x:xs) -> Select (gFromList (x:xs))
        Select (O (Nothing)) -> error "something wrong in tpageBBoxMapPDFBufFromTTempPageSelectPDFBuf"
        Select (O (Just _)) -> others 
      Select (O (Just sz)) = normalizedothers 
  in GPage (view g_dimension p) (view g_background p) (Select . O . Just $ replace s' sz)
       
-- | 
instance GCast TPageBBoxMapPDFBuf TTempPageSelectPDFBuf where
  gcast = ttempPageSelectPDFBufFromTPageBBoxMapPDFBuf

-- | 
ttempPageSelectPDFBufFromTPageBBoxMapPDFBuf :: TPageBBoxMapPDFBuf
                                               -> TTempPageSelectPDFBuf
ttempPageSelectPDFBufFromTPageBBoxMapPDFBuf p = 
  let normalizedothers = case (view g_layers p) of 
        NoSelect [] -> error "something wrong in ttemppageBBoxMapPDFBufFromTPageSelectPDFBuf" 
        NoSelect (x:xs) -> Select (gFromList (x:xs))
        Select (O (Nothing)) -> error "something wrong in ttemppageBBoxMapPDFBufFromTPageSelectPDFBuf"
        others@(Select (O (Just _))) -> others 
      Select (O (Just sz)) = normalizedothers 
      curr  = current sz 
      currtemp = GLayerBuf (view g_buffer curr) (TEitherAlterHitted . Left . gToList . view g_bstrokes $ curr)
  in  GPage (view g_dimension p) (view g_background p) 
            (TLayerSelectInPageBuf currtemp normalizedothers)

-- | 
instance GCast THoodleBBoxMapPDFBuf Hoodle where
  gcast = Hoodle <$> view g_title 
                  <*> gToList . fmap (toPageFromBuf gToBackground) . view g_pages 


-- | 
instance GCast TTempHoodleSelectPDFBuf Hoodle where 
  gcast = Hoodle <$> gselectTitle 
                  <*> gToList . fmap (toPageFromBuf gToBackground) . gselectAll  

----------------------      
----- Rendering   

-- | 
newtype InBBox a = InBBox a

-- | 
data InBBoxOption = InBBoxOption (Maybe BBox) 

-- | 
instance RenderOptionable (InBBox TLayerBBox) where
  type RenderOption (InBBox TLayerBBox) = InBBoxOption 
  cairoRenderOption (InBBoxOption mbbox) (InBBox layer) 
    = cairoDrawLayerBBox mbbox layer 

-- | 
instance RenderOptionable (InBBox TPageBBoxMapPDF) where
  type RenderOption (InBBox TPageBBoxMapPDF) = InBBoxOption 
  cairoRenderOption (InBBoxOption mbbox) (InBBox page) = do 
    cairoRenderOption (DrawPDFInBBox mbbox) (gbackground page, gdimension page) 
    mapM_ (cairoDrawLayerBBox mbbox) . glayers $ page 

-- |
mkTHoodleBBoxMapPDF :: Hoodle -> IO THoodleBBoxMapPDF
mkTHoodleBBoxMapPDF hdl = do 
  let pgs = hoodle_pages hdl
  npgs <- mkAllTPageBBoxMapPDF pgs 
  return $ GHoodle (hoodle_title hdl) (gFromList npgs)
  
-- |
mkAllTPageBBoxMapPDF :: [Page] -> IO [TPageBBoxMapPDF]
mkAllTPageBBoxMapPDF pgs = evalStateT (mapM mkPagePDF pgs) Nothing 

-- |
mkPagePDF :: Page -> StateT (Maybe Context) IO TPageBBoxMapPDF
mkPagePDF pg = do  
  let bkg = page_bkg pg
      dim = page_dim pg 
      ls = page_layers pg
  newbkg <- mkBkgPDF dim bkg
  return $ GPage dim newbkg (gFromList . Prelude.map fromLayer $ ls)
  
-- | 
mkBkgPDF :: Dimension -> Background 
            -> StateT (Maybe Context) IO BackgroundPDFDrawable
mkBkgPDF dim@(Dim w h) bkg = do  
  let bkgpdf = bkgPDFFromBkg bkg
  case bkgpdf of 
    BkgPDFSolid _c _s msfc -> do 
      case msfc of 
        Just _ -> return bkgpdf
        Nothing -> do 
          sfc <- liftIO $ createImageSurface FormatARGB32 (floor w) (floor h)
          renderWith sfc $ do 
            cairoDrawBkg dim (bkgFromBkgPDF bkgpdf) 
          return bkgpdf { bkgpdf_cairosurface = Just sfc}
    BkgPDFPDF md mf pn _ _ -> do 
#ifdef POPPLER
      mctxt <- St.get 
      case mctxt of
        Nothing -> do 
          case (md,mf) of 
            (Just d, Just f) -> do 
              mdoc <- popplerGetDocFromFile f
              put $ Just (Context d f mdoc)
              case mdoc of 
                Just doc -> do  
                  (mpg,msfc) <- popplerGetPageFromDoc doc pn 
                  return (bkgpdf {bkgpdf_popplerpage = mpg, bkgpdf_cairosurface = msfc}) 
                Nothing -> error "no pdf doc? in mkBkgPDF"
            _ -> return bkgpdf 
        Just (Context _oldd _oldf olddoc) -> do 
          (mpage,msfc) <- case olddoc of 
            Just doc -> do 
              popplerGetPageFromDoc doc pn
            Nothing -> return (Nothing,Nothing) 
          return $ BkgPDFPDF md mf pn mpage msfc
#else
          return bkgpdf
#endif  

-- | 
mkTLayerBBoxBufFromNoBuf :: Dimension -> TLayerBBox -> IO (TLayerBBoxBuf LyBuf)
mkTLayerBBoxBufFromNoBuf (Dim w h) lyr = do 
  let strs = view g_strokes lyr 
  sfc <- createImageSurface FormatARGB32 (floor w) (floor h)
  renderWith sfc (cairoDrawLayerBBox (Just (BBox (0,0) (w,h))) lyr)
  return $ GLayerBuf { gbuffer = LyBuf (Just sfc), 
                       gbstrokes = strs }  -- temporary

-- | 
updateLayerBuf :: Maybe BBox -> TLayerBBoxBuf LyBuf -> IO (TLayerBBoxBuf LyBuf)
updateLayerBuf mbbox lyr = do 
  case view g_buffer lyr of 
    LyBuf (Just sfc) -> do 
      renderWith sfc $ do 
        clearBBox mbbox        
        cairoDrawLayerBBox mbbox (gcast lyr :: TLayerBBox)
      return lyr
    _ -> return lyr
    
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

-- | 
resetPageBuffers :: TPageBBoxMapPDFBuf -> IO TPageBBoxMapPDFBuf 
resetPageBuffers page = do 
  let dim = view g_dimension page
      mbbox = Just . dimToBBox $ dim 
  newlayers <- mapM (updateLayerBuf mbbox) . view g_layers $ page 
  return (set g_layers newlayers page)

-- | 
resetHoodleBuffers :: THoodleBBoxMapPDFBuf -> IO THoodleBBoxMapPDFBuf 
resetHoodleBuffers hdl = do 
  let pages = view g_pages hdl 
  newpages <- mapM resetPageBuffers pages
  return . set g_pages newpages $ hdl
  
-- |
instance GCast (TLayerBBoxBuf a) TLayerBBox where
  gcast = tlayerBBoxFromTLayerBBoxBuf
 
-- |
instance GCast TPageBBoxMapPDFBuf TPageBBoxMapPDF where
  gcast = tpageBBoxMapPDFFromTPageBBoxMapPDFBuf

-- |
tlayerBBoxFromTLayerBBoxBuf :: TLayerBBoxBuf a -> TLayerBBox
tlayerBBoxFromTLayerBBoxBuf (GLayerBuf _ strs) = GLayer strs 

-- |
tpageBBoxMapPDFFromTPageBBoxMapPDFBuf :: TPageBBoxMapPDFBuf -> TPageBBoxMapPDF
tpageBBoxMapPDFFromTPageBBoxMapPDFBuf (GPage dim bkg ls) = 
  GPage dim bkg . gFromList . gToList . fmap tlayerBBoxFromTLayerBBoxBuf $ ls
 
-- | page within a bbox. not implemented bbox part.
cairoDrawPageBBoxPDF :: Maybe BBox -> TPageBBoxMapPDF -> Render ()
cairoDrawPageBBoxPDF _mbbox page = cairoRender page  -- This is temporary

-- | 
cairoDrawLayerBBoxBuf :: Maybe BBox -> TLayerBBoxBuf LyBuf -> Render ()
cairoDrawLayerBBoxBuf mbbox layer = do
  case view g_buffer layer of 
    LyBuf (Just sfc) -> do clipBBox mbbox 
                           setSourceSurface sfc 0 0 
                           -- setOperator OperatorSource
                           -- setAntialias AntialiasNone
                           paint 
                           resetClip 
    _ -> cairoDrawLayerBBox mbbox (gcast layer :: TLayerBBox)
  

-- |
instance RenderOptionable (InBBox (TLayerBBoxBuf LyBuf)) where
  type RenderOption (InBBox (TLayerBBoxBuf LyBuf)) = InBBoxOption 
  cairoRenderOption (InBBoxOption mbbox) (InBBox layer) 
    = cairoDrawLayerBBoxBuf mbbox layer
      
-- |      
instance RenderOptionable (InBBox TPageBBoxMapPDFBuf) where
  type RenderOption (InBBox TPageBBoxMapPDFBuf) = InBBoxOption 
  cairoRenderOption opt@(InBBoxOption mbbox) (InBBox page) = do 
    cairoRenderOption (DrawPDFInBBox mbbox) (view g_background page, view g_dimension page)
    mapM_ (cairoRenderOption opt . InBBox) .  view g_layers $ page


-- | deprecated 
tpageBBoxMapPDFFromTTempPageSelectPDF :: TTempPageSelectPDF -> TPageBBoxMapPDF
tpageBBoxMapPDFFromTTempPageSelectPDF p = 
  let TLayerSelectInPage s others = view g_layers p 
      s' = tlayerBBoxFromTLayerSelect s
  in GPage (view g_dimension p) (view g_background p) (gFromList (s':others))
