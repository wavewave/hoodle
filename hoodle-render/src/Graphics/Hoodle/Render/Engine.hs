{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.Engine
-- Copyright   : (c) 2014 Ian-Woo Kim
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
) where

import           Control.Concurrent.STM
import           Control.Monad (forever)
import qualified Data.HashMap.Strict as HM
import           Data.Sequence ( viewl, ViewL(..) )
import           Data.Time.Clock
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
pdfRendererMain :: ((SurfaceID,(Double,Cairo.Surface))->IO ()) -> PDFCommandQueue -> IO () 
pdfRendererMain handler tvar = forever $ do     
    p <- atomically $ do 
      lst' <- readTVar tvar
      case viewl lst' of
        EmptyL -> retry
        p :< ps -> do 
          writeTVar tvar ps 
          return p 
    pdfWorker handler (snd p)

pdfWorker :: ((SurfaceID,(Double,Cairo.Surface))->IO ()) 
          -> PDFCommand -> IO ()
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
    handler (sfcid,(s,sfc))

-- | generic renderer engine
genRendererMain :: TVar RenderCache -> ((SurfaceID,(Double,Cairo.Surface))->IO ()) 
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
    putStrLn $ "genRendererMain: " ++ show p
    genWorker cache handler (snd p)

genWorker :: RenderCache -> ((SurfaceID,(Double,Cairo.Surface))->IO ()) 
          -> GenCommand -> IO ()
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
    handler (sfcid,(s,sfc))
genWorker _cache handler (LayerInit sfcid ritms s (Dim w h)) = do
    sfc <- Cairo.createImageSurface Cairo.FormatARGB32 (floor w) (floor h)
    Cairo.renderWith sfc $ do   
      Cairo.setSourceRGBA 0 0 0 0
      Cairo.setOperator Cairo.OperatorSource
      Cairo.paint
      Cairo.setOperator Cairo.OperatorOver
      -- Cairo.rectangle 0 0 w h 
      -- Cairo.fill
      Cairo.scale s s
      mapM_ (renderRItem undefined undefined) ritms
    ctime <- getCurrentTime
    print ctime
      -- renderBkg (bkg,dim)
    handler (sfcid,(s,sfc))
genWorker cache handler (LayerRedraw sfcid ritms s (Dim w h)) = do
    case HM.lookup sfcid cache of
      Nothing -> do 
        ctime <- getCurrentTime
        putStrLn $ show ctime ++ ": Nothing"
        genWorker cache handler (LayerInit sfcid ritms s (Dim w h))
      Just (s,sfc) -> do
        ctime <- getCurrentTime
        putStrLn $ show ctime ++ ": Just " ++ show s
        Cairo.renderWith sfc $ do   
          Cairo.setSourceRGBA 0 0 0 0
          Cairo.setOperator Cairo.OperatorSource
          Cairo.paint
          Cairo.setOperator Cairo.OperatorOver
          Cairo.scale s s
          mapM_ (renderRItem undefined undefined) ritms
        handler (sfcid,(s,sfc))
genWorker _cache handler (LayerScaled sfcid ritms s (Dim w h)) = do
    sfc <- Cairo.createImageSurface Cairo.FormatARGB32 (floor w) (floor h)
    Cairo.renderWith sfc $ do   
      Cairo.setSourceRGBA 0 0 0 0
      Cairo.setOperator Cairo.OperatorSource
      Cairo.paint
      Cairo.setOperator Cairo.OperatorOver
      Cairo.scale s s
      mapM_ (renderRItem undefined undefined) ritms
    handler (sfcid,(s,sfc))


