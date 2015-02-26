{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.Engine
-- Copyright   : (c) 2014, 2015 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- rendering engine that can run as a separate thread
--
-----------------------------------------------------------------------------

module Graphics.Hoodle.Render.Engine
( pdfRendererMain
, genRendererMain
, transparentClear
) where

import           Control.Concurrent.STM
import           Control.Monad (forever)
import qualified Data.HashMap.Strict as HM
import           Data.Sequence ( viewl, ViewL(..) )
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.UI.Gtk.Poppler.Document as Poppler
import qualified Graphics.UI.Gtk.Poppler.Page as Poppler
--
import           Data.Hoodle.Simple
--
import           Graphics.Hoodle.Render
import           Graphics.Hoodle.Render.Type
import           Graphics.Hoodle.Render.Background

-- | main pdf renderer engine. need to be in a single thread
pdfRendererMain :: (RendererEvent->IO ()) -> PDFCommandQueue -> IO () 
pdfRendererMain handler tvar = forever $ do     
    p <- atomically $ do 
      lst' <- readTVar tvar
      case viewl lst' of
        EmptyL -> retry
        p :< ps -> do 
          writeTVar tvar ps 
          return p 
    pdfWorker handler (snd p)

pdfWorker :: (RendererEvent->IO ()) -> PDFCommand -> IO ()
pdfWorker _handler (GetDocFromFile fp tmvar) = do
    mdoc <- popplerGetDocFromFile fp
    atomically $ putTMVar tmvar mdoc 
pdfWorker _handler (GetDocFromDataURI str tmvar) = do
    mdoc <- popplerGetDocFromDataURI str
    atomically $ putTMVar tmvar mdoc
pdfWorker _handler (GetPageFromDoc doc pn tmvar) = do
    mpg <- popplerGetPageFromDoc doc pn
    atomically $ putTMVar tmvar mpg
pdfWorker _handler (GetNPages doc tmvar) = do
    n <- Poppler.documentGetNPages doc
    atomically $ putTMVar tmvar n
pdfWorker handler (RenderPageScaled sfcid page (Dim ow _oh) (Dim w h)) = do
    let s = w / ow
    sfc <- Cairo.createImageSurface Cairo.FormatARGB32 (floor w) (floor h)
    Cairo.renderWith sfc $ do   
      Cairo.setSourceRGBA 1 1 1 1
      Cairo.rectangle 0 0 w h 
      Cairo.fill
      Cairo.scale s s
      Poppler.pageRender page 
    handler (SurfaceUpdate (sfcid,(s,sfc)))

-- | generic renderer engine
genRendererMain :: TVar RenderCache -> (RendererEvent->IO ()) 
                -> GenCommandQueue -> IO () 
genRendererMain cachevar handler tvar = forever $ do     
    (p,cache) <- atomically $ do 
      cache <- readTVar cachevar
      lst' <- readTVar tvar
      case viewl lst' of
        EmptyL -> retry
        p :< ps -> do 
          writeTVar tvar ps 
          return (p,cache)
    genWorker cache handler (snd p)

genWorker :: RenderCache -> (RendererEvent->IO ()) -> GenCommand -> IO ()
genWorker _cache handler (BkgSmplScaled sfcid col sty dim@(Dim ow _oh) (Dim w h)) = do
    let s = w / ow
        bkg = Background "solid" col sty
    sfc <- Cairo.createImageSurface Cairo.FormatARGB32 (floor w) (floor h)
    Cairo.renderWith sfc $ do   
      Cairo.setSourceRGBA 1 1 1 1
      Cairo.rectangle 0 0 w h 
      Cairo.fill
      Cairo.scale s s
      renderBkg (bkg,dim)
    handler (SurfaceUpdate (sfcid,(s,sfc)))
genWorker _cache handler (LayerInit sfcid ritms) = do
    sfc <- Cairo.createImageSurface Cairo.FormatARGB32 612 792 -- (floor w) (floor h) -- for the time being
    Cairo.renderWith sfc $ do   
      Cairo.setSourceRGBA 0 0 0 0
      Cairo.setOperator Cairo.OperatorSource
      Cairo.paint
      Cairo.setOperator Cairo.OperatorOver
      mapM_ (renderRItem undefined undefined) ritms
    handler (SurfaceUpdate (sfcid,(1.0,sfc)))
genWorker cache handler (LayerRedraw sfcid ritms) = do
    case HM.lookup sfcid cache of
      Nothing -> genWorker cache handler (LayerInit sfcid ritms)
      Just (s,sfc) -> do
        Cairo.renderWith sfc $ do   
          Cairo.setSourceRGBA 0 0 0 0
          Cairo.setOperator Cairo.OperatorSource
          Cairo.paint
          Cairo.setOperator Cairo.OperatorOver
          Cairo.scale s s
          mapM_ (renderRItem undefined undefined) ritms
        handler (SurfaceUpdate (sfcid,(s,sfc)))
        handler (FinishCommandFor sfcid)
genWorker _cache handler (LayerScaled sfcid ritms (Dim ow _) (Dim w h)) = do
    let s = w / ow
    sfc <- Cairo.createImageSurface Cairo.FormatARGB32 (floor w) (floor h)
    Cairo.renderWith sfc $ do   
      Cairo.setSourceRGBA 0 0 0 0
      Cairo.setOperator Cairo.OperatorSource
      Cairo.paint
      Cairo.setOperator Cairo.OperatorOver
      Cairo.scale s s
      mapM_ (renderRItem undefined undefined) ritms
    handler (SurfaceUpdate (sfcid,(s,sfc)))

transparentClear :: Cairo.Render ()
transparentClear = do
  Cairo.setSourceRGBA 0 0 0 0
  Cairo.setOperator Cairo.OperatorSource
  Cairo.paint
  Cairo.setOperator Cairo.OperatorOver


