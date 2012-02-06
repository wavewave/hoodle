{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.Coroutine.Layer 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Application.HXournal.Coroutine.Layer where

import Application.HXournal.Type.Canvas
import Application.HXournal.Type.XournalState
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Alias 
import Application.HXournal.Accessor

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



import Data.IORef

import qualified Data.Sequence as Seq
import Graphics.UI.Gtk hiding (get,set)

layerAction :: (XournalState -> Int -> Page EditMode -> MainCoroutine XournalState) 
            -> MainCoroutine HXournalState
layerAction action = do 
    xst <- getSt 
    selectBoxAction (fsingle xst) (fsingle xst)  . get currentCanvasInfo $ xst
  where 
    fsingle xstate cvsInfo = do
      let epage = get currentPage cvsInfo
          cpn = get currentPageNum cvsInfo
          xojstate = get xournalstate xstate
      newxojstate <- either (action xojstate cpn) (action xojstate cpn . gcast) epage 
      return =<< (liftIO (updatePageAll newxojstate . set xournalstate newxojstate $ xstate))

-- | 

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
gotoNextLayer = layerAction nextlayeraction >>= putSt
  where nextlayeraction xojstate cpn page = do 
          let (_,currpage) = getCurrentLayerOrSet page
              Select (O (Just lyrzipper)) = get g_layers currpage  
          let mlyrzipper = moveRight lyrzipper 
          
              npage = maybe currpage (\x-> set g_layers (Select (O (Just x))) currpage) mlyrzipper
          case mlyrzipper of 
            Nothing -> liftIO $ putStrLn "Nothing"
            Just _ -> liftIO $ putStrLn "Just"
          let Select (O (Just ll)) = get g_layers npage
              SZ (_,(x1,x2)) = ll 
          return . setPageMap (M.adjust (const npage) cpn . getPageMap $ xojstate) $ xojstate  

gotoPrevLayer :: MainCoroutine ()
gotoPrevLayer = layerAction prevlayeraction >>= putSt
  where prevlayeraction xojstate cpn page = do 
          let (_,currpage) = getCurrentLayerOrSet page
              Select (O (Just lyrzipper)) = get g_layers currpage  
          let mlyrzipper = moveLeft lyrzipper 
              npage = maybe currpage (\x -> set g_layers (Select (O (Just x))) currpage) mlyrzipper
          let Select (O (Just ll)) = get g_layers npage
              SZ (_,(x1,x2)) = ll 
          case mlyrzipper of 
            Nothing -> liftIO $ putStrLn "Nothing"
            Just _ -> liftIO $ putStrLn "Just"
          return . setPageMap (M.adjust (const npage) cpn . getPageMap $ xojstate) $ xojstate  


gotoLayerAt :: Int -> MainCoroutine ()
gotoLayerAt n = layerAction gotoaction >>= putSt
  where gotoaction xojstate cpn page = do 
          let (_,currpage) = getCurrentLayerOrSet page
              Select (O (Just lyrzipper)) = get g_layers currpage  
          let mlyrzipper = moveTo n lyrzipper 
              npage = maybe currpage (\x -> set g_layers (Select (O (Just x))) currpage) mlyrzipper
          let Select (O (Just ll)) = get g_layers npage
              SZ (_,(x1,x2)) = ll 
          return . setPageMap (M.adjust (const npage) cpn . getPageMap $ xojstate) $ xojstate  


deleteCurrentLayer :: MainCoroutine ()
deleteCurrentLayer = layerAction deletelayeraction >>= commit
  where deletelayeraction xojstate cpn page = do 
          let (mcurrlayer,currpage) = getCurrentLayerOrSet page
          flip (maybe (return xojstate)) mcurrlayer $  
            const $ do 
              let Select (O (Just lyrzipper)) = get g_layers currpage  
                  mlyrzipper = deleteCurrent lyrzipper 
                  npage = maybe currpage 
                                (\x -> set g_layers (Select (O (Just x))) currpage) 
                                mlyrzipper
              return . setPageMap (M.adjust (const npage) cpn . getPageMap $ xojstate) $ xojstate  

startGotoLayerAt :: MainCoroutine ()
startGotoLayerAt = 
    selectBoxAction fsingle fsingle . get currentCanvasInfo =<< getSt
     {- (error "startGotoLayerAt") -}
  where 
    fsingle cvsInfo = do 
      let epage = get currentPage cvsInfo
          page = either id gcast epage 
          (_,currpage) = getCurrentLayerOrSet page
          Select (O (Just lyrzipper)) = get g_layers currpage
          cidx = currIndex lyrzipper
          len  = lengthSZ lyrzipper 
      lref <- liftIO $ newIORef cidx
      dialog <- liftIO (layerChooseDialog lref cidx len)
      res <- liftIO $ dialogRun dialog
      case res of 
        ResponseDeleteEvent -> liftIO $ widgetDestroy dialog
        ResponseOk ->  do
          liftIO $ widgetDestroy dialog
          newnum <- liftIO (readIORef lref)
          liftIO $ putStrLn (show (newnum))
          gotoLayerAt newnum
        ResponseCancel -> liftIO $ widgetDestroy dialog
        _ -> error "??? in fileOpen " 
      return ()

