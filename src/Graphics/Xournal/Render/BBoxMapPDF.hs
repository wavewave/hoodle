{-# LANGUAGE OverloadedStrings, TypeFamilies, TypeSynonymInstances, MultiParamTypeClasses,  
             FlexibleInstances, FlexibleContexts #-}

module Graphics.Xournal.Render.BBoxMapPDF where

import Graphics.Xournal.Render.Type 
import Graphics.Xournal.Render.Generic
import Graphics.Xournal.Render.PDFBackground
import Data.Xournal.Simple
import Data.Xournal.Generic
import Data.Xournal.Map
import Data.Xournal.BBox
import Data.Xournal.Buffer
import Data.Foldable 
import Data.IntMap
import Data.Traversable 

import Control.Category
import Control.Monad.State hiding (get, mapM_, mapM)
import qualified Control.Monad.State as St (get)
import Data.Label
import Prelude hiding ((.),id,mapM, mapM_ )

import Data.ByteString hiding (putStrLn, empty)
import qualified Data.ByteString.Char8 as C hiding (empty)
import qualified Graphics.UI.Gtk.Poppler.Document as Poppler

import Graphics.Xournal.Render.BBox
import Graphics.Rendering.Cairo


type TPageBBoxMapPDF = TPageBBoxMapBkg BackgroundPDFDrawable 

type TXournalBBoxMapPDF = TXournalBBoxMapBkg BackgroundPDFDrawable

type TTempPageSelectPDF = GPage BackgroundPDFDrawable (TLayerSelectInPage []) TLayerBBox

type TTempXournalSelectPDF = GSelect (IntMap TPageBBoxMapPDF) (Maybe (Int, TTempPageSelectPDF))


instance GBackgroundable BackgroundPDFDrawable where 
  gFromBackground = bkgPDFFromBkg 
  gToBackground = bkgFromBkgPDF



tlayerBBoxFromTLayerSelect :: TLayerSelect TLayerBBox -> TLayerBBox 
tlayerBBoxFromTLayerSelect l = 
  case unTEitherAlterHitted (gstrokes l) of
    Left strs -> GLayer strs 
    Right alist -> GLayer . Prelude.concat $ interleave id unHitted alist

tlayerbufFromTLayerSelectBuf :: TLayerSelectBuf (TLayerBBoxBuf b) 
                                -> (TLayerBBoxBuf b) 
tlayerbufFromTLayerSelectBuf l = 
  case unTEitherAlterHitted (get g_bstrokes l) of
    Left strs -> GLayerBuf (get g_buffer l) strs 
    Right alist -> GLayerBuf (get g_buffer l) . Prelude.concat 
                   $ interleave id unHitted alist
  
instance GCast TTempPageSelectPDF TPageBBoxMapPDF where      
  gcast = tpageBBoxMapPDFFromTTempPageSelectPDF


tpageBBoxMapPDFFromTTempPageSelectPDF :: TTempPageSelectPDF -> TPageBBoxMapPDF
tpageBBoxMapPDFFromTTempPageSelectPDF p = 
  let TLayerSelectInPage s others = glayers p 
      s' = tlayerBBoxFromTLayerSelect s
  in  GPage (gdimension p) (gbackground p) (gFromList (s':others))
     
instance GCast TTempPageSelectPDFBuf TPageBBoxMapPDFBuf where      
  gcast = tpageBBoxMapPDFBufFromTTempPageSelectPDFBuf
      
tpageBBoxMapPDFBufFromTTempPageSelectPDFBuf :: TTempPageSelectPDFBuf 
                                               -> TPageBBoxMapPDFBuf
tpageBBoxMapPDFBufFromTTempPageSelectPDFBuf p = 
  let TLayerSelectInPageBuf s others = get g_layers p 
      s' = tlayerbufFromTLayerSelectBuf s
  in  GPage (gdimension p) (gbackground p) (gFromList (s':others))

      
instance GCast TPageBBoxMapPDF TTempPageSelectPDF where
  gcast = ttempPageSelectPDFFromTPageBBoxMapPDF

ttempPageSelectPDFFromTPageBBoxMapPDF :: TPageBBoxMapPDF -> TTempPageSelectPDF 
ttempPageSelectPDFFromTPageBBoxMapPDF p = 
  let (x:xs) = gToList (glayers p)
      l = GLayer . TEitherAlterHitted . Left . gToList . gstrokes $ x
  in  GPage (gdimension p) (gbackground p) (TLayerSelectInPage l xs)
      

instance GCast TPageBBoxMapPDFBuf TTempPageSelectPDFBuf where
  gcast = ttempPageSelectPDFBufFromTPageBBoxMapPDFBuf

ttempPageSelectPDFBufFromTPageBBoxMapPDFBuf :: TPageBBoxMapPDFBuf
                                               -> TTempPageSelectPDFBuf
ttempPageSelectPDFBufFromTPageBBoxMapPDFBuf p = 
  let (x:xs) = gToList (get g_layers p)
      l = GLayerBuf { 
          gbuffer = gbuffer x ,
          gbstrokes = TEitherAlterHitted . Left . gToList . gbstrokes $ x
          }
  in  GPage (gdimension p) (gbackground p) (TLayerSelectInPageBuf l xs)

----------------------      




mkTXournalBBoxMapPDF :: Xournal -> IO TXournalBBoxMapPDF
mkTXournalBBoxMapPDF xoj = do 
  let pgs = xoj_pages xoj 
  npgs <- mkAllTPageBBoxMapPDF pgs 
  return $ GXournal (xoj_title xoj) (gFromList npgs)
  
mkAllTPageBBoxMapPDF :: [Page] -> IO [TPageBBoxMapPDF]
mkAllTPageBBoxMapPDF pgs = evalStateT (mapM mkPagePDF pgs) Nothing 


mkPagePDF :: Page -> StateT (Maybe Context) IO TPageBBoxMapPDF
mkPagePDF pg = do  
  let bkg = page_bkg pg
      dim = page_dim pg 
      ls = page_layers pg
  newbkg <- mkBkgPDF bkg
  return $ GPage dim newbkg (gFromList . Prelude.map fromLayer $ ls)
  
mkBkgPDF :: Background 
            -> StateT (Maybe Context) IO BackgroundPDFDrawable
mkBkgPDF bkg = do  
  let bkgpdf = bkgPDFFromBkg bkg
  case bkgpdf of 
    BkgPDFSolid _ _ -> return bkgpdf 
    BkgPDFPDF md mf pn _ _ -> do 
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
            _ -> return bkgpdf 
        Just (Context oldd oldf olddoc) -> do 
          (mpage,msfc) <- case olddoc of 
            Just doc -> do 
              popplerGetPageFromDoc doc pn
            Nothing -> return (Nothing,Nothing) 
          return $ BkgPDFPDF md mf pn mpage msfc



newtype LyBuf = LyBuf { mbuffer :: Maybe Surface } 

type instance StrokeTypeFromLayer (TLayerBBoxBuf b) = StrokeBBox 

type TPageBBoxMapPDFBuf = 
  TPageBBoxMapBkgBuf BackgroundPDFDrawable LyBuf
  
type TXournalBBoxMapPDFBuf = 
  TXournalBBoxMapBkgBuf BackgroundPDFDrawable LyBuf
  
type TTempPageSelectPDFBuf = 
  GPage BackgroundPDFDrawable (TLayerSelectInPageBuf []) (TLayerBBoxBuf LyBuf)

type TTempXournalSelectPDFBuf = 
  GSelect (IntMap TPageBBoxMapPDFBuf) (Maybe (Int, TTempPageSelectPDFBuf))

mkTLayerBBoxBufFromNoBuf :: Dimension -> TLayerBBox -> IO (TLayerBBoxBuf LyBuf)
mkTLayerBBoxBufFromNoBuf (Dim w h) lyr = do 
  let strs = get g_strokes lyr 
  sfc <- createImageSurface FormatARGB32 (floor w) (floor h)
  renderWith sfc (cairoDrawLayerBBox (Just (BBox (0,0) (w,h))) lyr)
  return $ GLayerBuf { gbuffer = LyBuf (Just sfc), 
                       gbstrokes = strs }  -- temporary

updateLayerBuf :: Maybe BBox -> TLayerBBoxBuf LyBuf -> IO (TLayerBBoxBuf LyBuf)
updateLayerBuf mbbox lyr = do 
  putStrLn "updatelayerbuf called"
  case get g_buffer lyr of 
    LyBuf (Just sfc) -> do 
      putStrLn "here"
      renderWith sfc (cairoDrawLayerBBox mbbox (gcast lyr :: TLayerBBox)) 
      return lyr
    _ -> return lyr
    

mkTPageBBoxMapPDFBufFromNoBuf :: TPageBBoxMapPDF -> IO TPageBBoxMapPDFBuf
mkTPageBBoxMapPDFBufFromNoBuf page = do 
  let dim = get g_dimension page
      bkg = get g_background page
      ls =  get g_layers page
  ls' <- mapM (mkTLayerBBoxBufFromNoBuf dim) ls
  return $ GPage dim bkg ls'
      

mkTXournalBBoxMapPDFBufFromNoBuf :: TXournalBBoxMapPDF 
                                    -> IO TXournalBBoxMapPDFBuf
mkTXournalBBoxMapPDFBufFromNoBuf xoj = do 
  let title = get g_title xoj
      pages = get g_pages xoj 
  pages' <- mapM mkTPageBBoxMapPDFBufFromNoBuf pages
 
  return $ GXournal title pages'

instance GCast (TLayerBBoxBuf a) TLayerBBox where
  gcast = tlayerBBoxFromTLayerBBoxBuf
 
instance GCast TPageBBoxMapPDFBuf TPageBBoxMapPDF where
  gcast = tpageBBoxMapPDFFromTPageBBoxMapPDFBuf

tlayerBBoxFromTLayerBBoxBuf :: TLayerBBoxBuf a -> TLayerBBox
tlayerBBoxFromTLayerBBoxBuf (GLayerBuf _ strs) = GLayer strs 

tpageBBoxMapPDFFromTPageBBoxMapPDFBuf :: TPageBBoxMapPDFBuf -> TPageBBoxMapPDF
tpageBBoxMapPDFFromTPageBBoxMapPDFBuf (GPage dim bkg ls) = 
  GPage dim bkg (fmap tlayerBBoxFromTLayerBBoxBuf ls)
 
  
----- Rendering   

newtype InBBox a = InBBox a

data InBBoxOption = InBBoxOption (Maybe BBox) 

instance RenderOptionable (InBBox TLayerBBox) where
  type RenderOption (InBBox TLayerBBox) = InBBoxOption 
  cairoRenderOption (InBBoxOption mbbox) (InBBox layer) 
    = cairoDrawLayerBBox mbbox layer 

                             
instance RenderOptionable (InBBox TPageBBoxMapPDF) where
  type RenderOption (InBBox TPageBBoxMapPDF) = InBBoxOption 
  cairoRenderOption opt@(InBBoxOption mbbox) (InBBox page) = do 
    cairoRenderOption (DrawPDFInBBox mbbox) (gbackground page, gdimension page) 
    mapM_ (cairoDrawLayerBBox mbbox) . glayers $ page 

    
-- | page within a bbox. not implemented bbox part.

cairoDrawPageBBoxPDF :: Maybe BBox -> TPageBBoxMapPDF -> Render ()
cairoDrawPageBBoxPDF mbbox page = cairoRender page  -- This is temporary




cairoDrawLayerBBoxBuf :: Maybe BBox -> TLayerBBoxBuf LyBuf -> Render ()
cairoDrawLayerBBoxBuf mbbox layer = do
  case get g_buffer layer of 
    LyBuf (Just sfc) -> do clipBBox mbbox 
                           setSourceSurface sfc 0 0 
                           paint 
                           resetClip 
    _ -> cairoDrawLayerBBox mbbox (gcast layer :: TLayerBBox)
  


instance RenderOptionable (InBBox (TLayerBBoxBuf LyBuf)) where
  type RenderOption (InBBox (TLayerBBoxBuf LyBuf)) = InBBoxOption 
  cairoRenderOption (InBBoxOption mbbox) (InBBox layer) 
    = cairoDrawLayerBBoxBuf mbbox layer
      
instance RenderOptionable (InBBox TPageBBoxMapPDFBuf) where
  type RenderOption (InBBox TPageBBoxMapPDFBuf) = InBBoxOption 
  cairoRenderOption opt@(InBBoxOption mbbox) (InBBox page) = do 
    cairoRenderOption (DrawPDFInBBox mbbox) (get g_background page, get g_dimension page)
    mapM_ (cairoRenderOption opt . InBBox) .  get g_layers $ page
