{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.Type.Background 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Graphics.Hoodle.Render.Type.Background where

import           Data.ByteString 
import           Graphics.Rendering.Cairo
#ifdef POPPLER
import qualified Graphics.UI.Gtk.Poppler.Document as Poppler
#endif
-- from hoodle-platform
import           Data.Hoodle.BBox
import           Data.Hoodle.Simple 
-- from this package

-- 
import Prelude hiding (mapM_)


-- | 
data Context = Context { ctxt_domain :: ByteString
                       , ctxt_filename :: ByteString 
#ifdef POPPLER
                       , ctxt_doc :: Maybe Poppler.Document
#else
                       , ctxt_doc :: Maybe ()
#endif 
                       }

-- |

data RBackground = RBkgSmpl 
                   { rbkg_color :: ByteString
                   , rbkg_style :: ByteString
                   , rbkg_cairosurface :: Maybe Surface }
                 | RBkgPDF 
                   { rbkg_domain :: Maybe ByteString
                   , rbkg_filename :: Maybe ByteString
                   , rbkg_pageno :: Int 
#ifdef POPPLER
                   , rbkg_popplerpage :: Maybe Poppler.Page 
#else    
                   , rbkg_popplerpage :: Maybe ()
#endif
                   , rbkg_cairosurface :: Maybe Surface } 

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
rbkg2Bkg (RBkgPDF d f n _ _ ) = BackgroundPdf "pdf" d f n 

-- |
bkg2RBkg :: Background -> RBackground
bkg2RBkg (Background _t c s) = RBkgSmpl c s Nothing
bkg2RBkg (BackgroundPdf _t md mf pn) = RBkgPDF md mf pn Nothing Nothing
