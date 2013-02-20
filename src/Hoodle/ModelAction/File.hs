{-# LANGUAGE OverloadedStrings, CPP, GADTs #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.ModelAction.File 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
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
-- import           Control.Category
import           Control.Lens
import           Control.Monad
import           Data.Attoparsec 
import qualified Data.ByteString as B
import           Data.Maybe 
import           Graphics.UI.Gtk hiding (get,set)
#ifdef POPPLER
import qualified Data.ByteString.Char8 as C
import qualified Graphics.UI.Gtk.Poppler.Document as Poppler
import qualified Graphics.UI.Gtk.Poppler.Page as PopplerPage
import           Graphics.Hoodle.Render.Background
#endif
import           System.FilePath (takeExtension)
-- from hoodle-platform 
import           Data.Hoodle.Simple
import           Graphics.Hoodle.Render
import qualified Text.Hoodle.Parse.Attoparsec as PA
import qualified Text.Hoodle.Migrate.V0_1_999_to_V0_1_9999 as MV
import qualified Text.Xournal.Parse.Conduit as XP
import           Text.Hoodle.Migrate.FromXournal
-- from this package
import           Hoodle.Type.HoodleState
-- 
-- import Prelude hiding ((.),id)


-- | check hoodle version and migrate if necessary 
checkVersionAndMigrate :: B.ByteString -> IO (Either String Hoodle) 
checkVersionAndMigrate bstr = do 
  case parseOnly PA.checkHoodleVersion bstr of 
    Left str -> error str 
    Right v -> do 
      if ( v < "0.1.9999" ) 
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
        bstr <- B.readFile fname
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
        mhdl <- makeNewHoodleWithPDF fname 
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

-- | 
makeNewHoodleWithPDF :: FilePath -> IO (Maybe Hoodle)
makeNewHoodleWithPDF fp = do 
#ifdef POPPLER
  let fname = C.pack fp 
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
            return (createPage dim fname i) 
      pgs <- mapM createPageAct [1..n]
      hdl <- set title fname . set pages pgs <$> emptyHoodle
      return (Just hdl)
#else
      return Nothing
#endif
      
-- | 
      
createPage :: Dimension -> B.ByteString -> Int -> Page
createPage dim fn n 
  | n == 1 = let bkg = BackgroundPdf "pdf" (Just "absolute") (Just fn ) n 
             in  Page dim bkg [emptyLayer]
  | otherwise = let bkg = BackgroundPdf "pdf" Nothing Nothing n 
                in Page dim bkg [emptyLayer]
                   
-- |                    

toggleSave :: UIManager -> Bool -> IO ()
toggleSave ui b = do 
    agr <- uiManagerGetActionGroups ui >>= \x -> 
      case x of
        [] -> error "No action group?"
        y:_ -> return y
    Just savea <- actionGroupGetAction agr "SAVEA"
    actionSetSensitive savea b
