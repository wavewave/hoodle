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
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Char8 as B
import           Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HM
import           Data.Sequence hiding (null,filter)
import qualified Data.Sequence as Seq (filter)
import           Data.UUID
import           Data.UUID.V4 (nextRandom)
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.UI.Gtk.Poppler.Document as Poppler
--
import           Data.Hoodle.Simple (Dimension(..))
--
import           Graphics.Hoodle.Render.Type.Item

newtype PDFCommandID = PDFCommandID UUID deriving (Show,Eq,Ord)

data PDFCommand where
  GetDocFromFile    :: B.ByteString -> TMVar (Maybe Poppler.Document) -> PDFCommand
  GetDocFromDataURI :: B.ByteString -> TMVar (Maybe Poppler.Document) -> PDFCommand 
  GetPageFromDoc    :: Poppler.Document -> !Int -> TMVar (Maybe Poppler.Page) -> PDFCommand 
  GetNPages :: !Poppler.Document -> (TMVar Int) -> PDFCommand
  RenderPageScaled  :: SurfaceID -> Poppler.Page -> Dimension -> Dimension -> PDFCommand 

instance Show PDFCommand where
  show _ = "PDFCommand"

newtype GenCommandID = GenCommandID UUID deriving (Show,Eq,Ord)

data GenCommand where
  BkgSmplScaled :: SurfaceID -> B.ByteString -> B.ByteString -> Dimension -> Dimension -> GenCommand
  LayerInit :: SurfaceID -> [RItem] -> GenCommand
  LayerRedraw :: SurfaceID -> [RItem] -> GenCommand
  LayerScaled :: SurfaceID -> [RItem] -> Dimension -> Dimension -> GenCommand



instance Show GenCommand where
  show (BkgSmplScaled sfcid _ _ _ _) = "BkgSmplScaled:"++show sfcid
  show (LayerInit sfcid _  ) = "LayerInit:"++show sfcid
  show (LayerRedraw sfcid _ ) = "LayerRedraw:"++show sfcid
  show (LayerScaled sfcid _ _ _ ) = "LayerScaled:"++show sfcid


newtype SurfaceID = SurfaceID UUID deriving (Show,Eq,Ord,Hashable)

-- |
type CanvasId = Int 

-- | hashmap: key = UUID, value = (original size, view size, surface)
type RenderCache = HM.HashMap SurfaceID (Double, Cairo.Surface)


-- |
data RendererEvent = SurfaceUpdate (SurfaceID, (Double,Cairo.Surface))
                   | FinishCommandFor SurfaceID

type PDFCommandQueue = TVar (Seq (PDFCommandID,PDFCommand))

type GenCommandQueue = TVar (Seq (GenCommandID,GenCommand))

data RendererState = RendererState { rendererHandler :: RendererEvent -> IO ()
                                   , rendererPDFCmdQ :: PDFCommandQueue
                                   , rendererGenCmdQ :: GenCommandQueue
                                   , rendererCache :: TVar RenderCache }

getRenderCache :: RendererState -> IO RenderCache
getRenderCache RendererState {..} = atomically (readTVar rendererCache)

type Renderer = ReaderT RendererState IO

issuePDFCommandID :: (Functor m, MonadIO m) => m PDFCommandID
issuePDFCommandID = PDFCommandID <$> liftIO nextRandom

issueGenCommandID :: (Functor m, MonadIO m) => m GenCommandID
issueGenCommandID = GenCommandID <$> liftIO nextRandom

issueSurfaceID :: (Functor m, MonadIO m) => m SurfaceID
issueSurfaceID = SurfaceID <$> liftIO nextRandom

sendPDFCommand :: PDFCommandQueue 
               -> PDFCommandID
               -> PDFCommand 
               -> STM ()
sendPDFCommand queuevar cmdid cmd = do
    queue <- readTVar queuevar
    let queue' = Seq.filter (not . isRemoved (cmdid,cmd)) queue
        nqueue = queue' |> (cmdid,cmd)
    writeTVar queuevar nqueue

isRemoved :: (PDFCommandID,PDFCommand) -> (PDFCommandID,PDFCommand) -> Bool
isRemoved (cmdid,ncmd) (ocmdid,ocmd) 
  | cmdid == ocmdid = True
  | otherwise = case ncmd of
                  RenderPageScaled nsfcid _ _ _ -> 
                    case ocmd of
                      RenderPageScaled osfcid _ _ _ -> nsfcid == osfcid
                      _ -> False
                  _ -> False


sendGenCommand :: GenCommandQueue 
               -> GenCommandID
               -> GenCommand 
               -> STM ()
sendGenCommand queuevar cmdid cmd = do
    queue <- readTVar queuevar
    let queue' = Seq.filter (not . isRemovedGen (cmdid,cmd)) queue 
        nqueue = queue' |> (cmdid,cmd)
    writeTVar queuevar nqueue

surfaceID :: GenCommand -> SurfaceID
surfaceID (BkgSmplScaled sfcid _ _ _ _) = sfcid
surfaceID (LayerInit sfcid _ ) = sfcid
surfaceID (LayerRedraw sfcid _ ) = sfcid
surfaceID (LayerScaled sfcid _ _ _ ) = sfcid


isRemovedGen :: (GenCommandID,GenCommand) -> (GenCommandID,GenCommand) -> Bool
isRemovedGen (cmdid,ncmd) (ocmdid,ocmd) 
  | cmdid == ocmdid = True
  | otherwise = case ncmd of
                  BkgSmplScaled nsfcid _ _ _ _ -> surfaceID ocmd == nsfcid
                  LayerScaled nsfcid _ _ _ -> surfaceID ocmd == nsfcid
                  _ -> False
