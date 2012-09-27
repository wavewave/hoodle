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
import           Control.Category
import           Control.Lens
import           Control.Monad
import           Data.Attoparsec 
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import           Data.Maybe 
import           Graphics.UI.Gtk hiding (get,set)
#ifdef POPPLER
import qualified Graphics.UI.Gtk.Poppler.Document as Poppler
import qualified Graphics.UI.Gtk.Poppler.Page as PopplerPage
#endif
import           System.FilePath (takeExtension)
-- from hoodle-platform 
import           Data.Hoodle.Simple
import           Graphics.Hoodle.Render
import           Graphics.Hoodle.Render.Background 
-- import qualified Text.Hoodle.Parse.Conduit as PC
import qualified Text.Hoodle.Parse.Attoparsec as PA
import qualified Text.Xournal.Parse.Conduit as XP
import           Text.Hoodle.Translate.FromXournal
-- from this package
import           Hoodle.Type.HoodleState
-- 
import Prelude hiding ((.),id)

-- | get file content from xournal file and update xournal state 

getFileContent :: Maybe FilePath 
               -> HoodleState 
               -> IO HoodleState 
getFileContent (Just fname) xstate = do 
    let ext = takeExtension fname
    if ext == ".hdl" 
      then do  
        bstr <- B.readFile fname
        let r = parse PA.hoodle bstr
        case r of 
          Done _ h -> constructNewHoodleStateFromHoodle h xstate 
                      >>= return . set currFileName (Just fname)
          _ -> print r >> return xstate 
      else if ext == ".xoj" 
        then do 
          XP.parseXojFile fname >>= \x -> case x of  
            Left str -> do
              putStrLn $ "file reading error : " ++ str 
              return xstate 
            Right xojcontent -> do 
              let hdlcontent = mkHoodleFromXournal xojcontent 
              nxstate <- constructNewHoodleStateFromHoodle hdlcontent xstate 
              return $ set currFileName (Just fname) nxstate               
        else getFileContent Nothing xstate      
getFileContent Nothing xstate = do   
    newhdl <- cnstrctRHoodle defaultHoodle 
    let newhdlstate = ViewAppendState newhdl 
        xstate' = set currFileName Nothing 
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
      pg <- Poppler.documentGetPage doc 0 
      (w,h) <- PopplerPage.pageGetSize pg
      let dim = Dim w h 
          hdl = set title fname 
              . set pages (map (createPage dim fname) [1..n]) 
              $ emptyHoodle
      return (Just hdl)
#else
  error "makeNewHoodleWithPDF should not be used without poppler lib"
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
