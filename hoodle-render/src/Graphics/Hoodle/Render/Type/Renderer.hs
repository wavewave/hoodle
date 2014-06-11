{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.Type.Renderer
-- Copyright   : (c) 2014 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Graphics.Hoodle.Render.Type.Renderer where

import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Char8 as B
import           Data.Sequence
import           Data.UUID
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.UI.Gtk.Poppler.Document as Poppler
-- import qualified Graphics.UI.Gtk.Poppler.Page as Poppler
--
import           Data.Hoodle.Simple (Dimension(..))


data PDFCommand = GetDocFromFile    B.ByteString         (TMVar (Maybe Poppler.Document))
                | GetDocFromDataURI B.ByteString         (TMVar (Maybe Poppler.Document))
                | GetPageFromDoc    Poppler.Document Int (TMVar (Maybe Poppler.Page))
                | RenderPageScaled  { pdfpage   :: Poppler.Page 
                                    , origsize  :: Dimension 
                                    , viewsize  :: Dimension 
                                    , resultbox :: TMVar Cairo.Surface }

instance Show PDFCommand where
  show _ = "PDFCommand"


type Renderer = ReaderT ((UUID, (Double,Cairo.Surface)) -> IO (), TVar (Seq (UUID, PDFCommand)), MVar ()) IO

