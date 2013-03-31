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
import           Data.Maybe 
import           Graphics.UI.Gtk hiding (get,set)
import           Data.ByteString.Base64 
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap as IM
import           Data.Monoid ((<>))
import qualified Graphics.UI.Gtk.Poppler.Document as Poppler
import qualified Graphics.UI.Gtk.Poppler.Page as PopplerPage
import           Graphics.Hoodle.Render.Background
import           System.Directory (canonicalizePath)
import           System.FilePath (takeExtension)
import           System.Process
-- from hoodle-platform 
import           Data.Hoodle.Generic
import           Data.Hoodle.Simple
import           Graphics.Hoodle.Render
import           Graphics.Hoodle.Render.Type.Background 
import           Graphics.Hoodle.Render.Type.Hoodle
import qualified Text.Hoodle.Parse.Attoparsec as PA
import qualified Text.Hoodle.Migrate.V0_1_1_to_V0_2 as MV
import qualified Text.Xournal.Parse.Conduit as XP
import           Text.Hoodle.Migrate.FromXournal
-- from this package
import           Hoodle.Type.HoodleState
-- 
-- import Prelude hiding ((.),id)


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
          Right h -> constructNewHoodleStateFromHoodle h xstate 
                     >>= return . set (hoodleFileControl.hoodleFileName) (Just fname)
      ".xoj" -> do 
          XP.parseXojFile fname >>= \x -> case x of  
            Left str -> do
              putStrLn $ "file reading error : " ++ str 
              return xstate 
            Right xojcontent -> do 
              hdlcontent <- mkHoodleFromXournal xojcontent 
              nxstate <- constructNewHoodleStateFromHoodle hdlcontent xstate 
              return $ set (hoodleFileControl.hoodleFileName) (Just fname) nxstate               
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
makeNewHoodleWithPDF doesembed fp = do 
  canonicalfp <- canonicalizePath fp 
  let fname = C.pack canonicalfp 
  mdoc <- popplerGetDocFromFile fname
  case mdoc of 
    Nothing -> do 
      putStrLn $ "no such file " ++ fp 
      return Nothing 
    Just doc -> do 
      n <- Poppler.documentGetNPages doc 

      let createPageAct i = do 
            pg <- Poppler.documentGetPage doc (i-1) 
            (w,h) <- PopplerPage.pageGetSize pg
            let dim = Dim w h 
            return (createPage doesembed dim fname i) 
      pgs <- mapM createPageAct [1..n]
      hdl <- set title fname . set pages pgs <$> emptyHoodle
      nhdl <- if doesembed 
                then do 
                  bstr <- C.readFile canonicalfp 
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
                   


-- | this function must be moved to GUI.Reflect
toggleSave :: UIManager -> Bool -> IO ()
toggleSave ui b = do 
    agr <- uiManagerGetActionGroups ui >>= \x -> 
      case x of
        [] -> error "No action group?"
        y:_ -> return y
    Just savea <- actionGroupGetAction agr "SAVEA"
    actionSetSensitive savea b
