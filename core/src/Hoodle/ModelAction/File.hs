{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches #-}

-- |
-- Module      : Hoodle.ModelAction.File
-- Copyright   : (c) 2011-2015 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
module Hoodle.ModelAction.File where

import Control.Lens (set, view)
import Control.Monad.IO.Class (liftIO)
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.ByteString.Base64 (encode)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Hoodle.Simple
  ( Background (..),
    Dimension (..),
    Hoodle,
    Image (..),
    Item (..),
    Page (..),
    embeddedPdf,
    emptyHoodle,
    emptyLayer,
    pages,
    title,
  )
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Time.Clock (getCurrentTime)
import qualified Data.Traversable as T
import Graphics.GD.ByteString
  ( imageSize,
    loadJpegFile,
    loadPngFile,
    savePngByteString,
  )
-- import Graphics.Hoodle.Render.Background (popplerGetDocFromFile)
import Graphics.Hoodle.Render.Type.Hoodle (rHoodle2Hoodle)
-- import qualified Graphics.UI.Gtk.Poppler.Document as Poppler
-- import qualified Graphics.UI.Gtk.Poppler.Page as PopplerPage
import Hoodle.Type.HoodleState
  ( FileStore (LocalDir, TempDir),
    UnitHoodle,
    getHoodle,
    hoodleFileControl,
    hoodleFileName,
    isSaved,
    lastSavedTime,
  )
import Hoodle.Util (mkTmpFile)
import System.Directory (canonicalizePath)
import System.FilePath (takeExtension)
import System.IO (IOMode (..), hClose, hFileSize, openFile)
import System.Process (readProcess)
import Text.Hoodle.Builder (builder)
import qualified Text.Hoodle.Migrate.V0_2_2_to_V0_3 as MV
import qualified Text.Hoodle.Migrate.V0_3_to_HEAD as MVHEAD
import qualified Text.Hoodle.Parse.Attoparsec as PA

-- | check hoodle version and migrate if necessary
checkVersionAndMigrate :: C.ByteString -> IO (Either String Hoodle)
checkVersionAndMigrate bstr = do
  case parseOnly PA.checkHoodleVersion bstr of
    Left str -> do
      liftIO $ putStrLn $ "checkVersionAndMigrate: " ++ show bstr
      error str
    Right v -> do
      if v <= "0.2.2"
        then T.traverse MVHEAD.hoodle2Hoodle =<< MV.migrate bstr
        else return (parseOnly PA.hoodle bstr)

-- | this is very temporary, need to be changed.
findFirstPDFFile :: [Page] -> Maybe C.ByteString
findFirstPDFFile xs =
  let ys = mapMaybe f xs
   in listToMaybe ys
  where
    f :: Page -> Maybe C.ByteString
    f p = case page_bkg p of
      BackgroundPdf _ _ fi _ -> fi
      _ -> Nothing

findAllPDFPages :: [Page] -> [Int]
findAllPDFPages = mapMaybe f
  where
    f p = case page_bkg p of
      BackgroundPdf _ _ _ n -> Just n
      _ -> Nothing

replacePDFPages :: [Page] -> [Page]
replacePDFPages = map f
  where
    f p =
      let bkg = page_bkg p
       in case bkg of
            BackgroundPdf typ _ _ pdfn -> p {page_bkg = BackgroundEmbedPdf typ pdfn}
            _ -> p

-- |
embedPDFInHoodle :: Hoodle -> IO Hoodle -- RHoodle -> IO RHoodle
embedPDFInHoodle hdl = putStrLn "embedPDFInHoodle is now bugful. I do not do anything here " >> return hdl

{-    let pgsWnum = zip [0..] (hoodle_pages hdl)
        mfn = findFirstPDFFile (hoodle_pages hdl)
        allpdfpg = findAllPDFPages pgs
    case mfn of
      Nothing -> return hdl
      Just fn -> do
        let fnstr = C.unpack fn
            pglst = map show allpdfpg
            cmdargs =  [fnstr, "cat"] ++ pglst ++ ["output", "-"]
        -- print cmdargs
        (_,Just hout,_,_) <- createProcess (proc "pdftk" cmdargs) { std_out = CreatePipe }
        bstr <- C.hGetContents hout
        let ebdsrc = makeEmbeddedPdfSrcString bstr
            npgs = (IM.fromAscList . replacePDFPages pgs

            nhdl0 = rHoodle2Hoodle . (gpages .~ npgs) $ hdl
            nhdl1 = nhdl0 { hoodle_embeddedpdf = Just ebdsrc }
        cnstrctRHoodle nhdl1
-}
{-
    let pgs = (IM.toAscList . view gpages) hdl
        mfn = findFirstPDFFile pgs
        allpdfpg = findAllPDFPages pgs

    case mfn of
      Nothing -> return hdl
      Just fn -> do
        let fnstr = C.unpack fn
            pglst = map show allpdfpg
            cmdargs =  [fnstr, "cat"] ++ pglst ++ ["output", "-"]
        print cmdargs
        (_,Just hout,_,_) <- createProcess (proc "pdftk" cmdargs) { std_out = CreatePipe }
        bstr <- C.hGetContents hout
        let ebdsrc = makeEmbeddedPdfSrcString bstr
            npgs = (IM.fromAscList . replacePDFPages) pgs
        (return . set gembeddedpdf (Just ebdsrc) . set gpages npgs) hdl
-}

makeEmbeddedPdfSrcString :: C.ByteString -> C.ByteString
makeEmbeddedPdfSrcString = ("data:application/x-pdf;base64," <>) . encode

-- |
makeNewHoodleWithPDF ::
  -- | doesEmbedPDF
  Bool ->
  -- | pdf file
  FilePath ->
  IO (Maybe Hoodle)
makeNewHoodleWithPDF doesembed ofp = do
  ocanonicalfp <- canonicalizePath ofp
  let ofname = C.pack ocanonicalfp
  let sizelimit = 10000000
  siz <- do
    h <- openFile ofp ReadMode
    s <- hFileSize h
    hClose h
    return s
  (nfp, nfname) <-
    if siz > sizelimit
      then do
        putStrLn $ "size is " ++ show siz ++ ", which is larger than " ++ show sizelimit
        nfp' <- mkTmpFile "pdf"
        let nfname' = C.pack nfp'
        _ <-
          readProcess
            "gs"
            [ "-q",
              "-dNOPAUSE",
              "-dBATCH",
              "-dSAFER",
              "-sDEVICE=pdfwrite",
              "-dCompatibilityLevel=1.3",
              "-dPDFSETTINGS=/screen",
              "-dEmbedAllFonts=true",
              "-dSubsetFonts=true",
              "-dColorImageDownsampleType=/Bicubic",
              "-dColorImageResolution=72",
              "-dGrayImageDownsampleType=/Bicubic",
              "-dGrayImageResolution=72",
              "-dMonoImageDownsampleType=/Bicubic",
              "-dMonoImageResolution=72",
              "-sOutputFile=" ++ nfp',
              ofp
            ]
            ""
        return (nfp', nfname')
      else return (ocanonicalfp, ofname)

  -- mdoc <- popplerGetDocFromFile nfname
  let mdoc = Nothing
  case mdoc of
    Nothing -> do
      putStrLn $ "no such file " ++ nfp
      pure Nothing
    Just doc -> do
      pure Nothing

{-      n <- Poppler.documentGetNPages doc

      let createPageAct i = do
            pg <- Poppler.documentGetPage doc (i - 1)
            (w, h) <- PopplerPage.pageGetSize pg
            let dim = Dim w h
            return (createPage doesembed dim nfname i)
      pgs <- mapM createPageAct [1 .. n]
      hdl <- set title nfname . set pages pgs <$> emptyHoodle
      nhdl <-
        if doesembed
          then do
            bstr <- C.readFile nfp
            let ebdsrc = makeEmbeddedPdfSrcString bstr
            return (set embeddedPdf (Just ebdsrc) hdl)
          else return hdl
      return (Just nhdl)
-}

-- |
createPage ::
  -- | does embed pdf?
  Bool ->
  Dimension ->
  C.ByteString ->
  Int ->
  Page
createPage doesembed dim fn n =
  let bkg
        | not doesembed && n == 1 =
          BackgroundPdf "pdf" (Just "absolute") (Just fn) n
        | not doesembed && n /= 1 =
          BackgroundPdf "pdf" Nothing Nothing n
        | otherwise -- doesembed
          =
          BackgroundEmbedPdf "embedpdf" n
   in Page dim bkg [emptyLayer]

-- |
saveHoodle :: UnitHoodle -> IO UnitHoodle
saveHoodle uhdl = do
  let hdl = (rHoodle2Hoodle . getHoodle) uhdl
  case view (hoodleFileControl . hoodleFileName) uhdl of
    LocalDir Nothing -> return uhdl
    LocalDir (Just filename) -> action hdl filename
    TempDir filename -> action hdl filename
  where
    action hdl filename = do
      L.writeFile filename . builder $ hdl
      ctime <- getCurrentTime
      return (set isSaved True . set (hoodleFileControl . lastSavedTime) (Just ctime) $ uhdl)

-- |
makeNewItemImage ::
  -- | isEmbedded?
  Bool ->
  FilePath ->
  IO Item
makeNewItemImage isembedded filename =
  if isembedded
    then
      let fileext = takeExtension filename
          imgaction
            | fileext == ".PNG" || fileext == ".png" = loadpng
            | fileext == ".JPG" || fileext == ".jpg" = loadjpg
            | otherwise = loadsrc
       in imgaction
    else loadsrc
  where
    loadsrc = (return . ItemImage) (Image (C.pack filename) (100, 100) (Dim 300 300))
    loadpng = do
      img <- loadPngFile filename
      (w, h) <- imageSize img
      let dim
            | w < 612 && h < 792 = Dim (fromIntegral w) (fromIntegral h)
            | w < 765 && h < 990 =
              Dim
                (fromIntegral w * 72 / 90)
                (fromIntegral h * 72 / 90)
            | w >= h = Dim 300 (fromIntegral h * 300 / fromIntegral w)
            | otherwise = Dim (fromIntegral w * 300 / fromIntegral h) 300
      bstr <- C.readFile filename
      let b64str = encode bstr
          ebdsrc = "data:image/png;base64," <> b64str
      return . ItemImage $ Image ebdsrc (50, 100) dim
    loadjpg = do
      img <- loadJpegFile filename
      (w, h) <- imageSize img
      let dim
            | w < 612 && h < 792 = Dim (fromIntegral w) (fromIntegral h)
            | w < 765 && h < 990 =
              Dim
                (fromIntegral w * 72 / 90)
                (fromIntegral h * 72 / 90)
            | w >= h = Dim 300 (fromIntegral h * 300 / fromIntegral w)
            | otherwise = Dim (fromIntegral w * 300 / fromIntegral h) 300
      bstr <- savePngByteString img
      let b64str = encode bstr
          ebdsrc = "data:image/png;base64," <> b64str
      return . ItemImage $ Image ebdsrc (50, 100) dim
