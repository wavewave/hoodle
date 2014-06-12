{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
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

-- import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Char8 as B
import           Data.Sequence hiding (null,filter)
import qualified Data.Sequence as Seq (null,filter)
import           Data.UUID
import           Data.UUID.V4 (nextRandom)
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.UI.Gtk.Poppler.Document as Poppler
--
import           Data.Hoodle.Simple (Dimension(..))


data PDFCommand where
  GetDocFromFile    :: !B.ByteString -> TMVar (Maybe Poppler.Document) -> PDFCommand
  GetDocFromDataURI :: !B.ByteString -> TMVar (Maybe Poppler.Document) -> PDFCommand 
  GetPageFromDoc    :: !Poppler.Document -> !Int -> TMVar (Maybe Poppler.Page) -> PDFCommand 
  RenderPageScaled  :: !Poppler.Page -> !Dimension -> !Dimension -> PDFCommand 

instance Show PDFCommand where
  show _ = "PDFCommand"


type Renderer = ReaderT ((UUID, (Double,Cairo.Surface)) -> IO (), TVar (Seq (UUID, PDFCommand))) IO


sendPDFCommand :: UUID -> TVar (Seq (UUID,PDFCommand)) -> PDFCommand -> STM ()
sendPDFCommand !uuid !queuevar !cmd = do
    queue <- readTVar queuevar
    let queue' = Seq.filter ((/=uuid) .fst) queue 
        nqueue = queue' |> (uuid,cmd)
    writeTVar queuevar nqueue
    --- when (Seq.null queue) (tryPutTMVar mvar () >> return ())
