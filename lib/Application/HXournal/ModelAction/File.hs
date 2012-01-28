{-# LANGUAGE OverloadedStrings, CPP, GADTs #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.ModelAction.File 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--

module Application.HXournal.ModelAction.File where

import Application.HXournal.Type.XournalState
import Application.HXournal.Type.Canvas
import Application.HXournal.ModelAction.Page
import Application.HXournal.Type.PageArrangement
import Application.HXournal.Util
import Data.Xournal.BBox
import qualified Text.Xournal.Parse as P
import qualified Data.IntMap as M
import Data.Maybe 

import Control.Monad
import Control.Category
import Data.Label
import Prelude hiding ((.),id)

import Data.Xournal.Map
import Data.Xournal.Simple
import Data.Xournal.Generic
import Graphics.Xournal.Render.Generic
import Graphics.Xournal.Render.BBoxMapPDF
import Graphics.Xournal.Render.PDFBackground

import Graphics.UI.Gtk hiding (get,set)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

#ifdef POPPLER
import qualified Graphics.UI.Gtk.Poppler.Document as Poppler
import qualified Graphics.UI.Gtk.Poppler.Page as PopplerPage
#endif

-- | get file content from xournal file and update xournal state 

getFileContent :: Maybe FilePath 
               -> HXournalState 
               -> IO HXournalState 
getFileContent (Just fname) xstate = do 
    xojcontent <- P.read_xournal fname 
    nxstate <- constructNewHXournalStateFromXournal xojcontent xstate 
    return $ set currFileName (Just fname) nxstate 
getFileContent Nothing xstate = do   
    newxoj <- mkTXournalBBoxMapPDFBufFromNoBuf <=< mkTXournalBBoxMapPDF 
              $ defaultXournal 
    let newxojstate = ViewAppendState newxoj 
        (_,ccinfo) = get currentCanvas xstate 
        xstate' = set currFileName Nothing 
                  . set xournalstate newxojstate
                  $ xstate 
        -- cmap = get canvasInfoMap xstate'
    let dim = get g_dimension . maybeError "getFileContent" . M.lookup 0 . get g_pages 
            $ newxoj 

        forSingle    = setPage newxojstate 0 
                     . set (pageOrigin.pageArrangement.viewInfo) (PageOrigin (0,0))
                     . set (pageDimension.pageArrangement.viewInfo) (PageDimension dim)
                     . modify (viewPortBBox.pageArrangement.viewInfo) (apply moveBBoxToOrigin)
                     . set currentPageNum 0 
    return (modifyCurrentCanvasInfo (selectBox forSingle (error "getFileContent")) xstate')

        {- ciupdt = setPage newxojstate 0                       
                 . modify viewInfo (gviewInfo ccinfo)
                   -- (ViewInfo OnePage Original (0,0) (w,h))
                 . set currentPageNum 0 
                 $ ccinfo
        cmap' = M.map ciupdt cmap 
    return (set canvasInfoMap cmap' xstate') -}

constructNewHXournalStateFromXournal :: Xournal -> HXournalState -> IO HXournalState 
constructNewHXournalStateFromXournal xoj' xstate = do 
    let currcid = get currentCanvas xstate 
        cmap = get canvasInfoMap xstate 
    xoj <- mkTXournalBBoxMapPDFBufFromNoBuf <=< mkTXournalBBoxMapPDF $ xoj'
    let dim = get g_dimension . maybeError "constructNewHxournalStateFromXournal" . M.lookup 0 
              . get g_pages $ xoj
        startingxojstate = ViewAppendState xoj
        forSingle = setPage startingxojstate 0 
                    . set (pageOrigin.pageArrangement.viewInfo) (PageOrigin (0,0)) 
                    . set (pageDimension.pageArrangement.viewInfo) (PageDimension dim)
                    . set currentPageNum 0 
    return $ set xournalstate startingxojstate
             . modifyCurrentCanvasInfo (selectBox forSingle (error "construct..."))
             $ xstate


{-    let changefunc c = 
          setPage startingxojstate 0 
          . set viewInfo (ViewInfo OnePage Original (0,0) (width,height))
          . set currentPageNum 0 
          $ c  
        cmap' = fmap changefunc cmap

             . set canvasInfoMap cmap'
             . set currentCanvas currcid -} 


makeNewXojWithPDF :: FilePath -> IO (Maybe Xournal)
makeNewXojWithPDF fp = do 
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
          xoj = set s_title fname 
                . set s_pages (map (createPage dim fname) [1..n]) 
                $ emptyXournal
      putStrLn $ "total num of pages " ++ show n 
      putStrLn $ "size = " ++ show (w,h)
      return (Just xoj)
#else
  error "makeNewXojWithPDF should not be used without poppler lib"
#endif
      
      
createPage :: Dimension -> B.ByteString -> Int -> Page
createPage dim fn n 
  | n == 1 = let bkg = BackgroundPdf "pdf" (Just "absolute") (Just fn ) n 
             in  Page dim bkg [emptyLayer]
  | otherwise = let bkg = BackgroundPdf "pdf" Nothing Nothing n 
                in Page dim bkg [emptyLayer]
                   
                   
toggleSave :: UIManager -> Bool -> IO ()
toggleSave ui b = do 
    agr <- uiManagerGetActionGroups ui >>= \x -> 
      case x of
        [] -> error "No action group?"
        y:_ -> return y
    Just savea <- actionGroupGetAction agr "SAVEA"
    actionSetSensitive savea b
