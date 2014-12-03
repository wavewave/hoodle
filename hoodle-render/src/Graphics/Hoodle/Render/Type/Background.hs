{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.Type.Background 
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Graphics.Hoodle.Render.Type.Background where

import           Data.ByteString 
import           Data.UUID
-- import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.UI.Gtk.Poppler.Document as Poppler
-- from hoodle-platform
import           Data.Hoodle.BBox
import           Data.Hoodle.Simple 
-- from this package

-- 
import Prelude hiding (mapM_)


-- | 
data Context = Context { ctxt_domain :: ByteString
                       , ctxt_filename :: ByteString 
                       , ctxt_doc :: Maybe Poppler.Document
                       , ctxt_embeddeddoc :: Maybe Poppler.Document
                       }

-- |

data RBackground = RBkgSmpl 
                   { rbkg_color :: ByteString
                   , rbkg_style :: ByteString
                   , rbkg_uuid :: UUID
                   }
                 | RBkgPDF 
                   { rbkg_domain :: Maybe ByteString
                   , rbkg_filename :: ByteString
                   , rbkg_pageno :: Int 
                   , rbkg_popplerpage :: Maybe Poppler.Page 
                   , rbkg_uuid :: UUID
                   } 
                 | RBkgEmbedPDF
                   { rbkg_pageno :: Int
                   , rbkg_popplerpage :: Maybe Poppler.Page 
                   , rbkg_uuid :: UUID
                   } 
instance Show (RBackground) where
  show _ = "RBackground"

isRBkgSmpl :: RBackground -> Bool 
isRBkgSmpl (RBkgSmpl _ _ _) = True 
isRBkgSmpl _ = False 

data RBkgOpt = RBkgDrawPDF 
             | RBkgDrawWhite 
             | RBkgDrawBuffer
             | RBkgDrawPDFInBBox (Maybe BBox)

-- |
rbkg2Bkg :: RBackground -> Background 
rbkg2Bkg (RBkgSmpl c s _) = Background "solid" c s 
rbkg2Bkg (RBkgPDF d f n _ _ ) = BackgroundPdf "pdf" d (Just f) n 
rbkg2Bkg (RBkgEmbedPDF n _ _) = BackgroundEmbedPdf "embedpdf" n
