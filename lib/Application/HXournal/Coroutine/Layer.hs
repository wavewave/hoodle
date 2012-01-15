{-# LANGUAGE ScopedTypeVariables #-}

module Application.HXournal.Coroutine.Layer where

import Application.HXournal.Type.Canvas
import Application.HXournal.Type.XournalState
import Application.HXournal.Type.Coroutine
import Application.HXournal.Accessor
import Application.HXournal.Util
import Application.HXournal.ModelAction.Layer
import Application.HXournal.ModelAction.Page
import Application.HXournal.Coroutine.Commit
import Control.Monad.Trans

import qualified Data.IntMap as M
import Data.Xournal.Generic
import Data.Xournal.Select
import Graphics.Xournal.Render.BBoxMapPDF
import Control.Compose
import Control.Category
import Data.Label
import Prelude hiding ((.),id)

import Data.Xournal.Simple

import Data.IORef

import qualified Data.Sequence as Seq
import Graphics.UI.Gtk hiding (get,set)

layerAction :: (XournalState -> Int -> TPageBBoxMapPDFBuf -> MainCoroutine XournalState) -> MainCoroutine HXournalState
layerAction action = do 
    xstate <- getSt
    let epage = get currentPage . getCurrentCanvasInfo $ xstate
        cpn = get currentPageNum . getCurrentCanvasInfo $ xstate
        xojstate = get xournalstate xstate
    newxojstate <- either (action xojstate cpn) (action xojstate cpn . gcast) epage 
    return . updatePageAll newxojstate . set xournalstate newxojstate $ xstate

makeNewLayer :: MainCoroutine () 
makeNewLayer = layerAction newlayeraction >>= commit 
  where newlayeraction xojstate cpn page = do 
          let (_,currpage) = getCurrentLayerOrSet page
              Select (O (Just lyrzipper)) = get g_layers currpage  
          emptylyr <- liftIO emptyTLayerBBoxBufLyBuf 
          let nlyrzipper = appendGoLast lyrzipper emptylyr 
              npage = set g_layers (Select (O (Just nlyrzipper))) currpage
          return . setPageMap (M.adjust (const npage) cpn . getPageMap $ xojstate) $ xojstate 
                

gotoNextLayer :: MainCoroutine ()
gotoNextLayer = layerAction nextlayeraction >> return ()
  where nextlayeraction xojstate cpn page = do 
          let (_,currpage) = getCurrentLayerOrSet page
              Select (O (Just lyrzipper)) = get g_layers currpage  
          let mlyrzipper = moveRight lyrzipper 
              npage = maybe currpage (\lyrzipper-> set g_layers (Select (O (Just lyrzipper))) currpage) mlyrzipper
          return . setPageMap (M.adjust (const npage) cpn . getPageMap $ xojstate) $ xojstate  

gotoPrevLayer :: MainCoroutine ()
gotoPrevLayer = layerAction prevlayeraction >> return ()
  where prevlayeraction xojstate cpn page = do 
          let (_,currpage) = getCurrentLayerOrSet page
              Select (O (Just lyrzipper)) = get g_layers currpage  
          let mlyrzipper = moveLeft lyrzipper 
              npage = maybe currpage (\lyrzipper-> set g_layers (Select (O (Just lyrzipper))) currpage) mlyrzipper
          return . setPageMap (M.adjust (const npage) cpn . getPageMap $ xojstate) $ xojstate  


gotoLayerAt :: Int -> MainCoroutine ()
gotoLayerAt n = layerAction gotoaction >> return () 
  where gotoaction xojstate cpn page = do 
          let (_,currpage) = getCurrentLayerOrSet page
              Select (O (Just lyrzipper)) = get g_layers currpage  
          let mlyrzipper = moveTo n lyrzipper 
              npage = maybe currpage (\lyrzipper-> set g_layers (Select (O (Just lyrzipper))) currpage) mlyrzipper
          return . setPageMap (M.adjust (const npage) cpn . getPageMap $ xojstate) $ xojstate  


deleteCurrentLayer :: MainCoroutine ()
deleteCurrentLayer = layerAction deletelayeraction >>= commit
  where deletelayeraction xojstate cpn page = do 
          let (mcurrlayer,currpage) = getCurrentLayerOrSet page
          case mcurrlayer of 
            Nothing -> return xojstate 
            Just currlayer -> do 
              let Select (O (Just lyrzipper)) = get g_layers currpage  
                  mlyrzipper = deleteCurrent lyrzipper 
                  npage = maybe currpage 
                                (\lyrzipper-> set g_layers (Select (O (Just lyrzipper))) currpage) 
                                mlyrzipper
              return . setPageMap (M.adjust (const npage) cpn . getPageMap $ xojstate) $ xojstate  

startGotoLayerAt :: MainCoroutine ()
startGotoLayerAt = do 
    liftIO $ putStrLn "startGotoLayerAt"
    xstate <- getSt 
    let epage = get currentPage . getCurrentCanvasInfo $ xstate 
        cpn = get currentPageNum . getCurrentCanvasInfo $ xstate
        xojstate = get xournalstate xstate 
        page = either id gcast epage 
        (_,currpage) = getCurrentLayerOrSet page
        Select (O (Just lyrzipper)) = get g_layers currpage
        cidx = currIndex lyrzipper
        len  = lengthSZ lyrzipper 
   
    lref <- liftIO $ newIORef cidx

    dialog <- liftIO (layerChooseDialog lref cidx len)
    {-    dialog <- liftIO $ dialogNew 
    layerentry <- liftIO $ entryNew
    -- label <- liftIO $ labelNew (Just "hello")
    button <- liftIO $buttonNewWithLabel "test"
    hbox <- liftIO $ hBoxNew 
    upper <- liftIO $ dialogGetUpper dialog
    liftIO $ boxPackStart upper hbox PackNatural 0 
    liftIO $ boxPackStart hbox layerentry PackNatural 
    liftIO $ boxPackStart hbox button PackGrow 0 


    liftIO $ widgetShowAll upper
    buttonOk <- liftIO $ dialogAddButton dialog stockOk ResponseOk
    buttonCancel <- liftIO $ dialogAddButton dialog stockCancel ResponseCancel -}
    res <- liftIO $ dialogRun dialog
    case res of 
        ResponseDeleteEvent -> liftIO $ widgetDestroy dialog
        ResponseOk ->  do
          liftIO $ putStrLn "ok!"
          liftIO $ widgetDestroy dialog
          newnum <- liftIO (readIORef lref)

          liftIO $ putStrLn (show (newnum))
        ResponseCancel -> liftIO $ widgetDestroy dialog
        _ -> error "??? in fileOpen " 
    return ()

