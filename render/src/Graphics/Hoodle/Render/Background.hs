{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches #-}

module Graphics.Hoodle.Render.Background where

import Control.Concurrent.STM
  ( atomically,
    newEmptyTMVarIO,
    takeTMVar,
  )
import Control.Monad (guard)
import Control.Monad.State (StateT, get, lift, liftIO, put)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Reader (ask)
import Data.ByteString (ByteString)
import Data.ByteString.Base64 (decode)
import qualified Data.ByteString.Char8 as C
import Data.Hoodle.BBox (BBox (..))
import Data.Hoodle.Predefined
  ( predefinedBkgcolor,
    predefinedRulingColor,
    predefinedRulingGraphSpacing,
    predefinedRulingLeftMargin,
    predefinedRulingMarginColor,
    predefinedRulingSpacing,
    predefinedRulingThickness,
    predefinedRulingTopMargin,
  )
import Data.Hoodle.Simple
  ( Background (..),
    Dimension (..),
  )
import qualified Data.Map as M
import Data.UUID.V4 (nextRandom)
import Graphics.Hoodle.Render.Type.Background
  ( Context (..),
    RBackground (..),
  )
import Graphics.Hoodle.Render.Type.Renderer
  ( PDFCommand (GetDocFromFile, GetPageFromDoc),
    Renderer,
    issuePDFCommandID,
    issueSurfaceID,
    rendererGenCmdQ,
    rendererPDFCmdQ,
    sendPDFCommand,
  )
import qualified Graphics.Rendering.Cairo as Cairo
-- import qualified Graphics.UI.Gtk.Poppler.Document as Poppler
-- import qualified Graphics.UI.Gtk.Poppler.Page as PopplerPage
import System.Directory
  ( getTemporaryDirectory,
    removeFile,
  )
import System.FilePath ((<.>), (</>))
import Prelude

-- |
-- popplerGetDocFromFile :: ByteString -> IO (Maybe Poppler.Document)
-- popplerGetDocFromFile fp =
--   Poppler.documentNewFromFile
--     (C.unpack ("file://localhost" `mappend` fp))
--     Nothing

-- |
getByteStringIfEmbeddedPDF :: ByteString -> Maybe ByteString
getByteStringIfEmbeddedPDF bstr = do
  guard (C.length bstr > 30)
  let (header, dat) = C.splitAt 30 bstr
  guard (header == "data:application/x-pdf;base64,")
  either (const Nothing) return (decode dat)

-- |
-- popplerGetDocFromDataURI :: ByteString -> IO (Maybe Poppler.Document)
-- popplerGetDocFromDataURI dat = do
--   let mdecoded = getByteStringIfEmbeddedPDF dat
--   case mdecoded of
--     Nothing -> return Nothing
--     Just decoded -> do
--       uuidstr <- fmap show nextRandom
--       tmpdir <- getTemporaryDirectory
--       let tmpfile = tmpdir </> uuidstr <.> "pdf"
--       C.writeFile tmpfile decoded
--       mdoc <- popplerGetDocFromFile (C.pack tmpfile)
--       removeFile tmpfile
--       return mdoc

-- |
-- popplerGetPageFromDoc ::
--   Poppler.Document ->
--   -- | page number
--   Int ->
--   IO (Maybe Poppler.Page)
-- popplerGetPageFromDoc doc pn = do
--   n <- Poppler.documentGetNPages doc
--   if pn > n
--     then return Nothing
--     else do
--       pg <- Poppler.documentGetPage doc (pn - 1)
--       return (Just pg)

-- | draw ruling all
drawRuling :: Double -> Double -> ByteString -> Cairo.Render ()
drawRuling w h style = do
  let drawHorizRules = do
        let (r, g, b, a) = predefinedRulingColor
        Cairo.setSourceRGBA r g b a
        Cairo.setLineWidth predefinedRulingThickness
        let drawonerule y = do
              Cairo.moveTo 0 y
              Cairo.lineTo w y
              Cairo.stroke
        mapM_
          drawonerule
          [ predefinedRulingTopMargin,
            predefinedRulingTopMargin + predefinedRulingSpacing
            .. h - 1
          ]
  case style of
    "plain" -> return ()
    "lined" -> do
      drawHorizRules
      let (r2, g2, b2, a2) = predefinedRulingMarginColor
      Cairo.setSourceRGBA r2 g2 b2 a2
      Cairo.setLineWidth predefinedRulingThickness
      Cairo.moveTo predefinedRulingLeftMargin 0
      Cairo.lineTo predefinedRulingLeftMargin h
      Cairo.stroke
    "ruled" -> drawHorizRules
    "graph" -> do
      let (r3, g3, b3, a3) = predefinedRulingColor
      Cairo.setSourceRGBA r3 g3 b3 a3
      Cairo.setLineWidth predefinedRulingThickness
      let drawonegraphvert x = do
            Cairo.moveTo x 0
            Cairo.lineTo x h
            Cairo.stroke
      let drawonegraphhoriz y = do
            Cairo.moveTo 0 y
            Cairo.lineTo w y
            Cairo.stroke
      mapM_ drawonegraphvert [0, predefinedRulingGraphSpacing .. w - 1]
      mapM_ drawonegraphhoriz [0, predefinedRulingGraphSpacing .. h - 1]
    _ -> return ()

-- | draw ruling  in bbox
drawRulingInBBox :: BBox -> Double -> Double -> ByteString -> Cairo.Render ()
drawRulingInBBox (BBox (x1, y1) (x2, y2)) w h style = do
  let drawonerule y = do
        Cairo.moveTo x1 y
        Cairo.lineTo x2 y
        Cairo.stroke
  let drawonegraphvert x = do
        Cairo.moveTo x y1
        Cairo.lineTo x y2
        Cairo.stroke
  let drawonegraphhoriz y = do
        Cairo.moveTo x1 y
        Cairo.lineTo x2 y
        Cairo.stroke
      fullRuleYs =
        [ predefinedRulingTopMargin,
          predefinedRulingTopMargin + predefinedRulingSpacing
          .. h - 1
        ]
      ruleYs = filter (\y -> (y <= y2) && (y >= y1)) fullRuleYs
      fullGraphXs = [0, predefinedRulingGraphSpacing .. w - 1]
      fullGraphYs = [0, predefinedRulingGraphSpacing .. h - 1]
      graphXs = filter (\x -> (x <= x2) && (x >= x1)) fullGraphXs
      graphYs = filter (\y -> (y <= y2) && (y >= y1)) fullGraphYs
  let drawHorizRules = do
        let (r, g, b, a) = predefinedRulingColor
        Cairo.setSourceRGBA r g b a
        Cairo.setLineWidth predefinedRulingThickness
        mapM_ drawonerule ruleYs
  case style of
    "plain" -> return ()
    "lined" -> do
      drawHorizRules
      let (r2, g2, b2, a2) = predefinedRulingMarginColor
      Cairo.setSourceRGBA r2 g2 b2 a2
      Cairo.setLineWidth predefinedRulingThickness
      Cairo.moveTo predefinedRulingLeftMargin 0
      Cairo.lineTo predefinedRulingLeftMargin h
      Cairo.stroke
    "ruled" -> drawHorizRules
    "graph" -> do
      let (r3, g3, b3, a3) = predefinedRulingColor
      Cairo.setSourceRGBA r3 g3 b3 a3
      Cairo.setLineWidth predefinedRulingThickness
      mapM_ drawonegraphvert graphXs
      mapM_ drawonegraphhoriz graphYs
    _ -> return ()

-- | render background without any constraint
renderBkg :: (Background, Dimension) -> Cairo.Render ()
renderBkg (Background _typ col sty, Dim w h) = do
  let c = M.lookup col predefinedBkgcolor
  case c of
    Just (r, g, b, _a) -> Cairo.setSourceRGB r g b
    Nothing -> Cairo.setSourceRGB 1 1 1
  Cairo.rectangle 0 0 w h
  Cairo.fill
  drawRuling w h sty
renderBkg (BackgroundPdf {}, Dim w h) = do
  Cairo.setSourceRGBA 1 1 1 1
  Cairo.rectangle 0 0 w h
  Cairo.fill
renderBkg (BackgroundEmbedPdf _ _, Dim w h) = do
  Cairo.setSourceRGBA 1 1 1 1
  Cairo.rectangle 0 0 w h
  Cairo.fill

-- | this has some bugs. need to fix
cnstrctRBkgStateT ::
  Dimension ->
  Background ->
  StateT (Maybe Context) Renderer RBackground
cnstrctRBkgStateT _ bkg = do
  (qpdf, _qgen) <- ((,) <$> rendererPDFCmdQ <*> rendererGenCmdQ) <$> lift ask
  sfcid <- issueSurfaceID
  case bkg of
    Background _t c s -> return (RBkgSmpl c s sfcid)
    BackgroundPdf _t md mf pn -> do
      r <- runMaybeT $ do
        (_pg, rbkg) <- case (md, mf) of
          (Just d, Just f) -> do
            cmdiddoc <- issuePDFCommandID
            docvar <- liftIO newEmptyTMVarIO
            liftIO . atomically $ sendPDFCommand qpdf cmdiddoc (GetDocFromFile f docvar)
            doc <- MaybeT . liftIO $ atomically $ takeTMVar docvar
            lift . put $ Just (Context d f (Just doc) Nothing)
            pg <- pdfRequest qpdf doc pn
            return (pg, RBkgPDF md f pn (Just pg) sfcid)
          _ -> do
            Context oldd oldf olddoc _ <- MaybeT get
            doc <- MaybeT . return $ olddoc
            pg <- pdfRequest qpdf doc pn
            return (pg, RBkgPDF (Just oldd) oldf pn (Just pg) sfcid)
        return rbkg
      case r of
        Nothing -> error "error in cnstrctRBkgStateT"
        Just x -> return x
    BackgroundEmbedPdf _ pn -> do
      r <- runMaybeT $ do
        Context _ _ _ mdoc <- MaybeT get
        doc <- (MaybeT . return) mdoc
        pg <- pdfRequest qpdf doc pn
        return (RBkgEmbedPDF pn (Just pg) sfcid)
      case r of
        Nothing -> error "error in cnstrctRBkgStateT"
        Just x -> return x
  where
    pdfRequest q doc pn = do
      cmdidpg <- issuePDFCommandID
      pgvar <- liftIO newEmptyTMVarIO
      liftIO . atomically $ sendPDFCommand q cmdidpg (GetPageFromDoc doc pn pgvar)
      MaybeT . liftIO $ atomically $ takeTMVar pgvar

-- | For simple hoodle background
renderBackgroundStateT :: Dimension -> Background -> StateT Context Cairo.Render ()
renderBackgroundStateT dim@(Dim w h) bkg = do
  case bkg of
    Background _t _c _s -> lift (renderBkg (bkg, dim))
    BackgroundPdf _t md mf pn -> do
      r <- runMaybeT $ do
        case (md, mf) of
          (Just d, Just f) -> do
            -- doc <- (MaybeT . liftIO . popplerGetDocFromFile) f
            let doc = ()
            lift . put $ Context d f (Just doc) Nothing
            -- pdfRenderDoc doc pn
            pure ()
          _ -> do
            Context _oldd _oldf olddoc _ <- lift get
            doc <- MaybeT . return $ olddoc
            -- pdfRenderDoc doc pn
            pure ()
      maybe (error "renderBackgroundStateT") (const (return ())) r
    BackgroundEmbedPdf _ pn -> do
      r <- runMaybeT $ do
        Context _ _ _ mdoc <- lift get
        doc <- (MaybeT . return) mdoc
        -- pdfRenderDoc doc pn
        pure ()
      maybe (error "renderBackgroundStateT") (const (return ())) r
  where

{-
pdfRender pg = do
  Cairo.setSourceRGBA 1 1 1 1
  Cairo.rectangle 0 0 w h
  Cairo.fill
  PopplerPage.pageRender pg
pdfRenderDoc doc pn =
  (MaybeT . liftIO) (popplerGetPageFromDoc doc pn)
    >>= lift . lift . pdfRender
-}
