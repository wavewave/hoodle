{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.Type.Renderer
-- Copyright   : (c) 2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Graphics.Hoodle.Render.Type.Renderer where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Char8 as B
import           Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HM
import           Data.Sequence hiding (null,filter)
import qualified Data.Sequence as Seq (null,filter)
import           Data.UUID
import           Data.UUID.V4 (nextRandom)
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.UI.Gtk.Poppler.Document as Poppler
--
import           Data.Hoodle.Simple (Dimension(..))


newtype PDFCommandID = PDFCommandID UUID
                     deriving (Show,Eq,Ord)

data PDFCommand where
  GetDocFromFile    :: !B.ByteString -> TMVar (Maybe Poppler.Document) -> PDFCommand
  GetDocFromDataURI :: !B.ByteString -> TMVar (Maybe Poppler.Document) -> PDFCommand 
  GetPageFromDoc    :: !Poppler.Document -> !Int -> TMVar (Maybe Poppler.Page) -> PDFCommand 
  GetNPages :: !Poppler.Document -> (TMVar Int) -> PDFCommand
  RenderPageScaled  :: !Poppler.Page -> !Dimension -> !Dimension -> PDFCommand 

instance Show PDFCommand where
  show _ = "PDFCommand"


newtype SurfaceID = SurfaceID UUID
                  deriving (Show,Eq,Ord,Hashable)

type Renderer = ReaderT ((SurfaceID, (Double,Cairo.Surface)) -> IO (), PDFCommandQueue) IO

-- | hashmap: key = UUID, value = (original size, view size, surface)
type RenderCache = HM.HashMap SurfaceID (Double, Cairo.Surface)

type PDFCommandQueue = TVar (Seq (PDFCommandID,PDFCommand))

issuePDFCommandID :: (Functor m, MonadIO m) => m PDFCommandID
issuePDFCommandID = PDFCommandID <$> liftIO nextRandom

issueSurfaceID :: (Functor m, MonadIO m) => m SurfaceID
issueSurfaceID = SurfaceID <$> liftIO nextRandom

sendPDFCommand :: PDFCommandID 
               -> PDFCommandQueue 
               -> PDFCommand 
               -> STM ()
sendPDFCommand !cmdid !queuevar !cmd = do
    queue <- readTVar queuevar
    let queue' = Seq.filter ((/=cmdid) .fst) queue 
        nqueue = queue' |> (cmdid,cmd)
    writeTVar queuevar nqueue

