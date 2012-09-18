{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- {-# LANGUAGE ExistentialQuantification, OverloadedStrings, 
--              FlexibleInstances, FlexibleContexts,  
--              TypeFamilies, CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.PDFBackground 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Graphics.Hoodle.Render.Background where

import Control.Monad.State hiding (mapM_)
import Data.Monoid 
import Data.ByteString hiding (putStrLn)
import qualified Data.ByteString.Char8 as C
import Graphics.Rendering.Cairo
--
#ifdef POPPLER
import qualified Graphics.UI.Gtk.Poppler.Document as Poppler
import qualified Graphics.UI.Gtk.Poppler.Page as PopplerPage
#endif
-- from hoodle-platform
import Data.Hoodle.Simple
-- from this package
--- import Graphics.Hoodle.Render.BBox 
-- import Graphics.Hoodle.Render.Generic
import Graphics.Hoodle.Render.Simple 
import Graphics.Hoodle.Render.Type.Background
--
import Prelude hiding (mapM_)
  
  
#ifdef POPPLER
popplerGetDocFromFile :: ByteString -> IO (Maybe Poppler.Document)
popplerGetDocFromFile fp = 
  Poppler.documentNewFromFile 
    (C.unpack ("file://localhost" `mappend` fp)) Nothing 
#endif

#ifdef POPPLER             
popplerGetPageFromDoc :: Poppler.Document 
                      -> Int -- ^ page number 
                      -> IO (Maybe Poppler.Page, Maybe Surface)
popplerGetPageFromDoc doc pn = do   
  -- n <- Poppler.documentGetNPages doc  
  -- putStrLn $ "pages : " ++ (show n)
  -- putStrLn $ "current page = " ++ show pn
  pg <- Poppler.documentGetPage doc (pn-1) 
  (w,h) <- PopplerPage.pageGetSize pg
  sfc <- createImageSurface FormatARGB32 (floor w) (floor h)
  renderWith sfc $ do   
    setSourceRGBA 1 1 1 1
    rectangle 0 0 w h 
    fill
    PopplerPage.pageRender pg
  return (Just pg, Just sfc)
#endif

renderRBkg :: (RBackground,Dimension) 
              -> Render ()
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

{-
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
-}
      

