{-# LANGUAGE ExistentialQuantification, OverloadedStrings, 
             FlexibleInstances, FlexibleContexts,  
             TypeFamilies, CPP #-}

module Graphics.Xournal.Render.PDFBackground where

import Control.Monad.State hiding (mapM_)
import Prelude hiding (mapM_)

import Data.Foldable 
import Data.Monoid 
import Data.ByteString hiding (putStrLn)
import qualified Data.ByteString.Char8 as C
import Data.Xournal.Generic
import Data.Xournal.Simple
import Data.Xournal.BBox
import Graphics.Xournal.Render.BBox 
import Graphics.Xournal.Render.Generic
import Graphics.Xournal.Render.Type

#ifdef POPPLER
import qualified Graphics.UI.Gtk.Poppler.Document as Poppler
import qualified Graphics.UI.Gtk.Poppler.Page as PopplerPage
#endif

import Graphics.Rendering.Cairo

  
data Context = Context { ctxt_domain :: ByteString
                       , ctxt_filename :: ByteString 
#ifdef POPPLER
                       , ctxt_doc :: Maybe Poppler.Document
#else
                       , ctxt_doc :: Maybe ()
#endif 
                       }

data BackgroundPDFDrawable = 
  BkgPDFSolid { bkgpdf_color :: ByteString
              , bkgpdf_style :: ByteString
              }
  | BkgPDFPDF { bkgpdf_domain :: Maybe ByteString
              , bkgpdf_filename :: Maybe ByteString
              , bkgpdf_pageno :: Int 
#ifdef POPPLER
              , bkgpdf_popplerpage :: Maybe Poppler.Page 
#else    
              , bkgpdf_popplerpage :: Maybe ()
#endif
              , bkgpdf_cairosurface :: Maybe Surface
              } 

data BkgPDFOption = DrawBkgPDF 
                  | DrawWhite 
                  | DrawBuffer
                  | DrawPDFInBBox (Maybe BBox)

bkgFromBkgPDF :: BackgroundPDFDrawable -> Background 
bkgFromBkgPDF (BkgPDFSolid c s) = Background "solid" c s 
bkgFromBkgPDF (BkgPDFPDF d f n _ _ ) = BackgroundPdf "pdf" d f n 

bkgPDFFromBkg :: Background -> BackgroundPDFDrawable
bkgPDFFromBkg (Background _t c s) = BkgPDFSolid c s
bkgPDFFromBkg (BackgroundPdf _t md mf pn) = BkgPDFPDF md mf pn Nothing Nothing

            
#ifdef POPPLER
popplerGetDocFromFile :: (MonadIO m) => 
                         ByteString -> m (Maybe Poppler.Document)
popplerGetDocFromFile fp = 
  liftIO $ Poppler.documentNewFromFile 
             (C.unpack ("file://localhost" `mappend` fp)) Nothing 
#endif

#ifdef POPPLER             
popplerGetPageFromDoc :: (MonadIO m) => 
                         Poppler.Document -> Int -> m (Maybe Poppler.Page, Maybe Surface)
popplerGetPageFromDoc doc pn = do   
  n <- liftIO $ Poppler.documentGetNPages doc  
  liftIO $ putStrLn $ "pages : " ++ (show n)
  liftIO $ putStrLn $ "current page = " ++ show pn
  pg <- liftIO $ Poppler.documentGetPage doc (pn-1) 
  (w,h) <- liftIO $ PopplerPage.pageGetSize pg
  sfc <- liftIO $ createImageSurface FormatARGB32 (floor w) (floor h)
  renderWith sfc $ do   
    setSourceRGBA 1 1 1 1
    rectangle 0 0 w h 
    fill
    PopplerPage.pageRender pg
  return (Just pg, Just sfc)
#endif

cairoRenderBackgroundPDFDrawable :: (BackgroundPDFDrawable,Dimension) 
                                    -> Render ()
cairoRenderBackgroundPDFDrawable (BkgPDFSolid c s,dim) = 
  cairoRender (Background "solid" c s,dim)
cairoRenderBackgroundPDFDrawable (BkgPDFPDF _ _ _ p _,dim) = do
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

instance Renderable (BackgroundPDFDrawable,Dimension) where
  cairoRender = cairoRenderBackgroundPDFDrawable


instance RenderOptionable (BackgroundPDFDrawable,Dimension) where
  type RenderOption (BackgroundPDFDrawable,Dimension) = BkgPDFOption 
  cairoRenderOption DrawBkgPDF (b,dim) = cairoRenderBackgroundPDFDrawable (b,dim)
  cairoRenderOption DrawWhite (_,Dim w h) = do 
    setSourceRGBA 1 1 1 1
    rectangle 0 0 w h 
    fill 
  cairoRenderOption DrawBuffer (b,dim) = do 
    case b of 
      BkgPDFSolid _ _ -> cairoRenderOption DrawBkgPDF (b,dim)
      BkgPDFPDF _ _ _ _ msfc -> do 
        case bkgpdf_cairosurface b of 
          Nothing -> cairoRenderOption DrawBkgPDF (b,dim)
          Just sfc -> do 
            setSourceSurface sfc 0 0 
            paint 
  cairoRenderOption (DrawPDFInBBox mbbox) (b,dim) = do 
    case b of 
      BkgPDFSolid _ _ -> cairoDrawBackgroundBBox mbbox dim (bkgFromBkgPDF b)
      BkgPDFPDF _ _ _ _ _ -> cairoRenderOption DrawBuffer (b,dim)

      

