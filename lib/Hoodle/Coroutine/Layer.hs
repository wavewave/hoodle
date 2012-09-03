{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Layer 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Layer where

import Control.Monad.State
import qualified Data.IntMap as M
import Control.Compose
import Control.Category
-- import Data.Label
import Control.Lens 
import Data.IORef
import Graphics.UI.Gtk hiding (get,set)
--
import Data.Xournal.Generic
import Data.Xournal.Select
import Graphics.Xournal.Render.BBoxMapPDF
-- 
import Hoodle.Type.Canvas
import Hoodle.Type.XournalState
import Hoodle.Type.Coroutine
import Hoodle.Type.Alias 
import Hoodle.Accessor
import Hoodle.ModelAction.Layer
import Hoodle.ModelAction.Page
import Hoodle.Coroutine.Commit
-- 
import Prelude hiding ((.),id)


layerAction :: (XournalState -> Int -> Page EditMode -> MainCoroutine XournalState) 
            -> MainCoroutine HoodleState
layerAction action = do 
    xst <- get 
    selectBoxAction (fsingle xst) (fsingle xst)  . view currentCanvasInfo $ xst
  where 
    fsingle xstate cvsInfo = do
      let epage = getCurrentPageEitherFromXojState cvsInfo xojstate
          cpn = view currentPageNum cvsInfo
          xojstate = view xournalstate xstate
      newxojstate <- either (action xojstate cpn) (action xojstate cpn . gcast) epage 
      return =<< (liftIO (updatePageAll newxojstate . set xournalstate newxojstate $ xstate))

-- | 

makeNewLayer :: MainCoroutine () 
makeNewLayer = layerAction newlayeraction >>= commit 
  where newlayeraction xojstate cpn page = do 
          let (_,currpage) = getCurrentLayerOrSet page
              Select (O (Just lyrzipper)) = view g_layers currpage  
          emptylyr <- liftIO emptyTLayerBBoxBufLyBuf 
          let nlyrzipper = appendGoLast lyrzipper emptylyr 
              npage = set g_layers (Select (O (Just nlyrzipper))) currpage
          return . setPageMap (M.adjust (const npage) cpn . getPageMap $ xojstate) $ xojstate 
                

gotoNextLayer :: MainCoroutine ()
gotoNextLayer = layerAction nextlayeraction >>= put
  where nextlayeraction xojstate cpn page = do 
          let (_,currpage) = getCurrentLayerOrSet page
              Select (O (Just lyrzipper)) = view g_layers currpage  
          let mlyrzipper = moveRight lyrzipper 
          
              npage = maybe currpage (\x-> set g_layers (Select (O (Just x))) currpage) mlyrzipper
          case mlyrzipper of 
            Nothing -> liftIO $ putStrLn "Nothing"
            Just _ -> liftIO $ putStrLn "Just"
          -- let Select (O (Just ll)) = view g_layers npage
          --     SZ (_,(x1,x2)) = ll 
          return . setPageMap (M.adjust (const npage) cpn . getPageMap $ xojstate) $ xojstate  

gotoPrevLayer :: MainCoroutine ()
gotoPrevLayer = layerAction prevlayeraction >>= put
  where prevlayeraction xojstate cpn page = do 
          let (_,currpage) = getCurrentLayerOrSet page
              Select (O (Just lyrzipper)) = view g_layers currpage  
          let mlyrzipper = moveLeft lyrzipper 
              npage = maybe currpage (\x -> set g_layers (Select (O (Just x))) currpage) mlyrzipper
          -- let Select (O (Just ll)) = view g_layers npage
          --     SZ (_,(x1,x2)) = ll 
          case mlyrzipper of 
            Nothing -> liftIO $ putStrLn "Nothing"
            Just _ -> liftIO $ putStrLn "Just"
          return . setPageMap (M.adjust (const npage) cpn . getPageMap $ xojstate) $ xojstate  


gotoLayerAt :: Int -> MainCoroutine ()
gotoLayerAt n = layerAction gotoaction >>= put
  where gotoaction xojstate cpn page = do 
          let (_,currpage) = getCurrentLayerOrSet page
              Select (O (Just lyrzipper)) = view g_layers currpage  
          let mlyrzipper = moveTo n lyrzipper 
              npage = maybe currpage (\x -> set g_layers (Select (O (Just x))) currpage) mlyrzipper
          -- let Select (O (Just ll)) = view g_layers npage
          --     SZ (_,(_x1,_x2)) = ll 
          return . setPageMap (M.adjust (const npage) cpn . getPageMap $ xojstate) $ xojstate  


deleteCurrentLayer :: MainCoroutine ()
deleteCurrentLayer = layerAction deletelayeraction >>= commit
  where deletelayeraction xojstate cpn page = do 
          let (mcurrlayer,currpage) = getCurrentLayerOrSet page
          flip (maybe (return xojstate)) mcurrlayer $  
            const $ do 
              let Select (O (Just lyrzipper)) = view g_layers currpage  
                  mlyrzipper = deleteCurrent lyrzipper 
                  npage = maybe currpage 
                                (\x -> set g_layers (Select (O (Just x))) currpage) 
                                mlyrzipper
              return . setPageMap (M.adjust (const npage) cpn . getPageMap $ xojstate) $ xojstate  

startGotoLayerAt :: MainCoroutine ()
startGotoLayerAt = 
    selectBoxAction fsingle fsingle . view currentCanvasInfo =<< get
  where 
    fsingle cvsInfo = do 
      xstate <- get 
      let xojstate = view xournalstate xstate
      let epage = getCurrentPageEitherFromXojState cvsInfo xojstate
          page = either id gcast epage 
          (_,currpage) = getCurrentLayerOrSet page
          Select (O (Just lyrzipper)) = view g_layers currpage
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

