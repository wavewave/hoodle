{-# LANGUAGE ExistentialQuantification, OverloadedStrings, 
             FlexibleInstances, FlexibleContexts,  
             TypeFamilies, CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Xournal.Render.PDFBackground 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--

module Graphics.Xournal.Render.PDFBackground where


import Control.Monad.State hiding (mapM_)
import Prelude hiding (mapM_)


import Data.Monoid 
import Data.ByteString hiding (putStrLn)
import qualified Data.ByteString.Char8 as C

import Data.Xournal.Simple
import Data.Xournal.BBox
import Graphics.Xournal.Render.BBox 
import Graphics.Xournal.Render.Generic

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
              , bkgpdf_cairosurface :: Maybe Surface
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
bkgFromBkgPDF (BkgPDFSolid c s _) = Background "solid" c s 
bkgFromBkgPDF (BkgPDFPDF d f n _ _ ) = BackgroundPdf "pdf" d f n 

bkgPDFFromBkg :: Background -> BackgroundPDFDrawable
bkgPDFFromBkg (Background _t c s) = BkgPDFSolid c s Nothing
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
cairoRenderBackgroundPDFDrawable (BkgPDFSolid c s _,dim) = 
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
      BkgPDFSolid _ _ msfc  -> do  
        case msfc of 
          Nothing -> cairoRenderOption DrawBkgPDF (b,dim)
          Just sfc -> do 
            setSourceSurface sfc 0 0 
            -- setOperator OperatorSource
            -- setAntialias AntialiasNone
            paint 
      BkgPDFPDF _ _ _ _ msfc -> do 
        case msfc of 
          Nothing -> cairoRenderOption DrawBkgPDF (b,dim)
          Just sfc -> do 
            setSourceSurface sfc 0 0 
            -- setOperator OperatorSource
            -- setAntialias AntialiasNone
            paint 
  cairoRenderOption (DrawPDFInBBox mbbox) (b,dim) = do 
    case b of 
      BkgPDFSolid _ _ _ -> do 
        clipBBox mbbox
        cairoRenderOption DrawBuffer (b,dim)
        resetClip
      BkgPDFPDF _ _ _ _ _ -> do 
        clipBBox mbbox
        cairoRenderOption DrawBuffer (b,dim)
        resetClip

      

