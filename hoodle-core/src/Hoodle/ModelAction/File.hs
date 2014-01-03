{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.ModelAction.File 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.ModelAction.File where

-- from other package
import           Control.Applicative
import           Control.Lens (view,set)
import           Data.Attoparsec 
import           Data.ByteString.Base64 
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.IntMap as IM
import           Data.Maybe 
import           Data.Monoid ((<>))
import           Data.Time.Clock
import           Graphics.GD.ByteString 
import           Graphics.UI.Gtk hiding (get,set)
import qualified Graphics.UI.Gtk.Poppler.Document as Poppler
import qualified Graphics.UI.Gtk.Poppler.Page as PopplerPage
import           System.Directory (canonicalizePath)
import           System.FilePath (takeExtension)
import           System.IO (hClose, hFileSize, openFile, IOMode(..)) 
import           System.Process
-- from hoodle-platform 
import           Data.Hoodle.Generic
import           Data.Hoodle.Simple
import           Graphics.Hoodle.Render
import           Graphics.Hoodle.Render.Background
import           Graphics.Hoodle.Render.Type.Background 
import           Graphics.Hoodle.Render.Type.Hoodle
import           Text.Hoodle.Builder (builder)
import qualified Text.Hoodle.Parse.Attoparsec as PA
import qualified Text.Hoodle.Migrate.V0_1_1_to_V0_2 as MV
import qualified Text.Xournal.Parse.Conduit as XP
import           Text.Hoodle.Migrate.FromXournal
-- from this package
import           Hoodle.Type.HoodleState
import           Hoodle.Util

-- | check hoodle version and migrate if necessary 
checkVersionAndMigrate :: C.ByteString -> IO (Either String Hoodle) 
checkVersionAndMigrate bstr = do 
  case parseOnly PA.checkHoodleVersion bstr of 
    Left str -> error str 
    Right v -> do 
      if ( v <= "0.1.1" ) 
        then MV.migrate bstr
        else return (parseOnly PA.hoodle bstr)

-- | get file content from xournal file and update xournal state 
getFileContent :: Maybe FilePath 
               -> HoodleState 
               -> IO HoodleState 
getFileContent (Just fname) xstate = do 
    let ext = takeExtension fname
    case ext of 
      ".hdl" -> do 
        bstr <- C.readFile fname
        r <- checkVersionAndMigrate bstr 
        case r of 
          Left err -> putStrLn err >> return xstate 
          Right h -> do 
            nxstate <- constructNewHoodleStateFromHoodle h xstate 
            ctime <- getCurrentTime
            return . set (hoodleFileControl.hoodleFileName) (Just fname)
                   . set (hoodleFileControl.lastSavedTime) (Just ctime) $ nxstate
      ".xoj" -> do 
          XP.parseXojFile fname >>= \x -> case x of  
            Left str -> do
              putStrLn $ "file reading error : " ++ str 
              return xstate 
            Right xojcontent -> do 
              hdlcontent <- mkHoodleFromXournal xojcontent 
              nxstate <- constructNewHoodleStateFromHoodle hdlcontent xstate 
              ctime <- getCurrentTime 
              return . set (hoodleFileControl.hoodleFileName) (Just fname) 
                     . set (hoodleFileControl.lastSavedTime) (Just ctime) $ nxstate               
      ".pdf" -> do 
        let doesembed = view (settings.doesEmbedPDF) xstate
        mhdl <- makeNewHoodleWithPDF doesembed fname 
        case mhdl of 
          Nothing -> getFileContent Nothing xstate 
          Just hdl -> do 
            newhdlstate <- constructNewHoodleStateFromHoodle hdl xstate 
            return . set (hoodleFileControl.hoodleFileName) Nothing $ newhdlstate 
      _ -> getFileContent Nothing xstate      
getFileContent Nothing xstate = do   
    newhdl <- cnstrctRHoodle =<< defaultHoodle 
    let newhdlstate = ViewAppendState newhdl 
        xstate' = set (hoodleFileControl.hoodleFileName) Nothing 
                  . set hoodleModeState newhdlstate
                  $ xstate 
    return xstate' 

-- |
constructNewHoodleStateFromHoodle :: Hoodle -> HoodleState -> IO HoodleState 
constructNewHoodleStateFromHoodle hdl' xstate = do 
    hdl <- cnstrctRHoodle hdl'
    let startinghoodleModeState = ViewAppendState hdl
    return $ set hoodleModeState startinghoodleModeState xstate

-- | this is very temporary, need to be changed.     
findFirstPDFFile :: [(Int,RPage)] -> Maybe C.ByteString
findFirstPDFFile xs = let ys = (filter isJust . map f) xs 
                      in if null ys then Nothing else head ys 
  where f (_,p) = case view gbackground p of 
                    RBkgPDF _ fi _ _ _ -> Just fi
                    _ -> Nothing 
      
findAllPDFPages :: [(Int,RPage)] -> [Int]
findAllPDFPages = catMaybes . map f
  where f (n,p) = case view gbackground p of 
                    RBkgPDF _ _ _ _ _ -> Just n
                    _ -> Nothing 

replacePDFPages :: [(Int,RPage)] -> [(Int,RPage)] 
replacePDFPages xs = map f xs 
  where f (n,p) = case view gbackground p of 
                    RBkgPDF _ _ pdfn mpdf msfc -> (n, set gbackground (RBkgEmbedPDF pdfn mpdf msfc) p)
                    _ -> (n,p) 
        
-- | 
embedPDFInHoodle :: RHoodle -> IO RHoodle
embedPDFInHoodle hdl = do 
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



makeEmbeddedPdfSrcString :: C.ByteString -> C.ByteString 
makeEmbeddedPdfSrcString = ("data:application/x-pdf;base64," <>) . encode

-- | 
makeNewHoodleWithPDF :: Bool              -- ^ doesEmbedPDF
                     -> FilePath          -- ^ pdf file
                     -> IO (Maybe Hoodle) 
makeNewHoodleWithPDF doesembed ofp = do 
  ocanonicalfp <- canonicalizePath ofp 
  let ofname = C.pack ocanonicalfp 
  let sizelimit = 10000000  
  siz <- do     
    h <- openFile ofp ReadMode
    s <- hFileSize h
    hClose h
    return s
  (nfp,nfname) <- if (siz > sizelimit) 
                    then do putStrLn $ "size is " ++ show siz ++ ", which is larger than " ++ show sizelimit
                            nfp' <- mkTmpFile "pdf"
                            let nfname' = C.pack nfp' 
                            readProcess "gs" [ "-q", "-dNOPAUSE", "-dBATCH", "-dSAFER"
                                             , "-sDEVICE=pdfwrite", "-dCompatibilityLevel=1.3"
                                             , "-dPDFSETTINGS=/screen", "-dEmbedAllFonts=true" 
                                             , "-dSubsetFonts=true", "-dColorImageDownsampleType=/Bicubic"
                                             , "-dColorImageResolution=72", "-dGrayImageDownsampleType=/Bicubic"
                                             , "-dGrayImageResolution=72", "-dMonoImageDownsampleType=/Bicubic"
                                             , "-dMonoImageResolution=72", "-sOutputFile="++nfp'
                                             , ofp ] "" 
                            return (nfp',nfname')
                    else return (ocanonicalfp,ofname) 
       
  mdoc <- popplerGetDocFromFile nfname
  case mdoc of 
    Nothing -> do 
      putStrLn $ "no such file " ++ nfp 
      return Nothing 
    Just doc -> do 
      n <- Poppler.documentGetNPages doc 

      let createPageAct i = do 
            pg <- Poppler.documentGetPage doc (i-1) 
            (w,h) <- PopplerPage.pageGetSize pg
            let dim = Dim w h 
            return (createPage doesembed dim nfname i) 
      pgs <- mapM createPageAct [1..n]
      hdl <- set title nfname . set pages pgs <$> emptyHoodle
      nhdl <- if doesembed 
                then do 
                  bstr <- C.readFile nfp 
                  let ebdsrc = makeEmbeddedPdfSrcString bstr 
                  return (set embeddedPdf (Just ebdsrc) hdl)
                else return hdl 
      return (Just nhdl)
      
-- | 
createPage :: Bool         -- ^ does embed pdf?
           -> Dimension 
           -> C.ByteString 
           -> Int 
           -> Page
createPage doesembed dim fn n =
    let bkg   
          | not doesembed && n == 1 
            = BackgroundPdf "pdf" (Just "absolute") (Just fn ) n 
          | not doesembed && n /= 1 
            = BackgroundPdf "pdf" Nothing Nothing n 
          | otherwise -- doesembed 
            = BackgroundEmbedPdf "embedpdf" n 
    in Page dim bkg [emptyLayer]
                   

-- | 
saveHoodle :: HoodleState -> IO HoodleState 
saveHoodle xstate = do 
    let hdl = (rHoodle2Hoodle . getHoodle) xstate 
    case view (hoodleFileControl.hoodleFileName) xstate of 
      Nothing -> return xstate 
      Just filename -> do 
        L.writeFile filename . builder $ hdl
        ctime <- getCurrentTime 
        let ui = view gtkUIManager xstate
        toggleSave ui False
        return (set isSaved True . set (hoodleFileControl.lastSavedTime) (Just ctime) $ xstate )
             
-- | this function must be moved to GUI.Reflect
toggleSave :: UIManager -> Bool -> IO ()
toggleSave ui b = do 
    agr <- uiManagerGetActionGroups ui >>= \x -> 
      case x of
        [] -> error "No action group?"
        y:_ -> return y
    Just savea <- actionGroupGetAction agr "SAVEA"
    actionSetSensitive savea b

-- | 
makeNewItemImage :: Bool  -- ^ isEmbedded?
                    -> FilePath 
                    -> IO Item
makeNewItemImage isembedded filename = 
    if isembedded 
      then let fileext = takeExtension filename 
               imgaction 
                 | fileext == ".PNG" || fileext == ".png" = loadpng 
                 | fileext == ".JPG" || fileext == ".jpg" = loadjpg 
                 | otherwise = loadsrc 
           in imgaction 
      else loadsrc 
  where loadsrc = (return . ItemImage) (Image (C.pack filename) (100,100) (Dim 300 300))
        loadpng = do 
          img <- loadPngFile filename
          (w,h) <- imageSize img 
          let dim | w >= h = Dim 300 (fromIntegral h*300/fromIntegral w)
                  | otherwise = Dim (fromIntegral w*300/fromIntegral h) 300 
          -- bstr <- savePngByteString img 
          bstr <- C.readFile filename 
          let b64str = encode bstr 
              ebdsrc = "data:image/png;base64," <> b64str
          return . ItemImage $ Image ebdsrc (100,100) dim 
        loadjpg = do 
          img <- loadJpegFile filename
          (w,h) <- imageSize img 
          let dim | w >= h = Dim 300 (fromIntegral h*300/fromIntegral w)
                  | otherwise = Dim (fromIntegral w*300/fromIntegral h) 300 
          bstr <- savePngByteString img 
          let b64str = encode bstr 
              ebdsrc = "data:image/png;base64," <> b64str
          return . ItemImage $ Image ebdsrc (100,100) dim 

