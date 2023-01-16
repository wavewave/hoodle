{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Graphics.Hoodle.Render.Type.Background where

import Data.ByteString (ByteString)
import Data.Hoodle.BBox (BBox)
import Data.Hoodle.Simple (Background (..))
import Graphics.Hoodle.Render.Type.Renderer (SurfaceID)
-- import qualified Graphics.UI.Gtk.Poppler.Document as Poppler
--
import Prelude hiding (mapM_)

-- |
data Context = Context
  { ctxt_domain :: ByteString,
    ctxt_filename :: ByteString,
    ctxt_doc :: Maybe (), -- Maybe Poppler.Document,
    ctxt_embeddeddoc :: Maybe () -- Poppler.Document
  }

-- |
data RBackground
  = RBkgSmpl
      { rbkg_color :: ByteString,
        rbkg_style :: ByteString,
        rbkg_surfaceid :: SurfaceID -- UUID
      }
  | RBkgPDF
      { rbkg_domain :: Maybe ByteString,
        rbkg_filename :: ByteString,
        rbkg_pageno :: Int,
        rbkg_popplerpage :: Maybe (), -- Maybe Poppler.Page,
        rbkg_surfaceid :: SurfaceID -- UUID
      }
  | RBkgEmbedPDF
      { rbkg_pageno :: Int,
        rbkg_popplerpage :: Maybe (), -- Maybe Poppler.Page,
        rbkg_surfaceid :: SurfaceID -- UUID
      }

instance Show RBackground where
  show _ = "RBackground"

isRBkgSmpl :: RBackground -> Bool
isRBkgSmpl RBkgSmpl {} = True
isRBkgSmpl _ = False

data RBkgOpt
  = RBkgDrawPDF
  | RBkgDrawWhite
  | RBkgDrawBuffer
  | RBkgDrawPDFInBBox (Maybe BBox)

-- |
rbkg2Bkg :: RBackground -> Background
rbkg2Bkg (RBkgSmpl c s _) = Background "solid" c s
rbkg2Bkg (RBkgPDF d f n _ _) = BackgroundPdf "pdf" d (Just f) n
rbkg2Bkg (RBkgEmbedPDF n _ _) = BackgroundEmbedPdf "embedpdf" n
