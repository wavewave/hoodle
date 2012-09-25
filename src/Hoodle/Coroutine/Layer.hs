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
import Data.Hoodle.Generic
import Data.Hoodle.Zipper
import Graphics.Hoodle.Render.Type 
-- 
import Hoodle.Accessor
import Hoodle.Coroutine.Commit
import Hoodle.ModelAction.Layer
import Hoodle.ModelAction.Page
import Hoodle.Type.Alias 
import Hoodle.Type.Canvas
import Hoodle.Type.Coroutine
import Hoodle.Type.HoodleState
-- 
import Prelude hiding ((.),id)


layerAction :: (HoodleModeState -> Int -> Page EditMode -> MainCoroutine HoodleModeState) 
            -> MainCoroutine HoodleState
layerAction action = do 
    xst <- get 
    selectBoxAction (fsingle xst) (fsingle xst)  . view currentCanvasInfo $ xst
  where 
    fsingle xstate cvsInfo = do
      let epage = getCurrentPageEitherFromHoodleModeState cvsInfo hdlmodst
          cpn = view currentPageNum cvsInfo
          hdlmodst = view hoodleModeState xstate
      newhdlmodst <- either (action hdlmodst cpn) (action hdlmodst cpn . hPage2RPage) epage 
      return =<< (liftIO (updatePageAll newhdlmodst . set hoodleModeState newhdlmodst $ xstate))

-- | 

makeNewLayer :: MainCoroutine () 
makeNewLayer = layerAction newlayeraction >>= commit 
  where newlayeraction hdlmodst cpn page = do 
          let (_,currpage) = getCurrentLayerOrSet page
              Select (O (Just lyrzipper)) = view glayers currpage  
          -- emptylyr <- liftIO ( emptyRLayer) 
          let emptylyr = emptyRLayer 
          let nlyrzipper = appendGoLast lyrzipper emptylyr 
              npage = set glayers (Select (O (Just nlyrzipper))) currpage
          return . setPageMap (M.adjust (const npage) cpn . getPageMap $ hdlmodst) $ hdlmodst 
                

gotoNextLayer :: MainCoroutine ()
gotoNextLayer = layerAction nextlayeraction >>= put
  where nextlayeraction hdlmodst cpn page = do 
          let (_,currpage) = getCurrentLayerOrSet page
              Select (O (Just lyrzipper)) = view glayers currpage  
          let mlyrzipper = moveRight lyrzipper 
          
              npage = maybe currpage (\x-> set glayers (Select (O (Just x))) currpage) mlyrzipper
          case mlyrzipper of 
            Nothing -> liftIO $ putStrLn "Nothing"
            Just _ -> liftIO $ putStrLn "Just"
          return . setPageMap (M.adjust (const npage) cpn . getPageMap $ hdlmodst) $ hdlmodst  

gotoPrevLayer :: MainCoroutine ()
gotoPrevLayer = layerAction prevlayeraction >>= put
  where prevlayeraction hdlmodst cpn page = do 
          let (_,currpage) = getCurrentLayerOrSet page
              Select (O (Just lyrzipper)) = view glayers currpage  
          let mlyrzipper = moveLeft lyrzipper 
              npage = maybe currpage (\x -> set glayers (Select (O (Just x))) currpage) mlyrzipper
          case mlyrzipper of 
            Nothing -> liftIO $ putStrLn "Nothing"
            Just _ -> liftIO $ putStrLn "Just"
          return . setPageMap (M.adjust (const npage) cpn . getPageMap $ hdlmodst) $ hdlmodst  


gotoLayerAt :: Int -> MainCoroutine ()
gotoLayerAt n = layerAction gotoaction >>= put
  where gotoaction hdlmodst cpn page = do 
          let (_,currpage) = getCurrentLayerOrSet page
              Select (O (Just lyrzipper)) = view glayers currpage  
          let mlyrzipper = moveTo n lyrzipper 
              npage = maybe currpage (\x -> set glayers (Select (O (Just x))) currpage) mlyrzipper
          return . setPageMap (M.adjust (const npage) cpn . getPageMap $ hdlmodst) $ hdlmodst  


deleteCurrentLayer :: MainCoroutine ()
deleteCurrentLayer = layerAction deletelayeraction >>= commit
  where deletelayeraction hdlmodst cpn page = do 
          let (mcurrlayer,currpage) = getCurrentLayerOrSet page
          flip (maybe (return hdlmodst)) mcurrlayer $  
            const $ do 
              let Select (O (Just lyrzipper)) = view glayers currpage  
                  mlyrzipper = deleteCurrent lyrzipper 
                  npage = maybe currpage 
                            (\x -> set glayers (Select (O (Just x))) currpage) 
                            mlyrzipper
              return . setPageMap (M.adjust (const npage) cpn . getPageMap $ hdlmodst) $ hdlmodst  

startGotoLayerAt :: MainCoroutine ()
startGotoLayerAt = 
    selectBoxAction fsingle fsingle . view currentCanvasInfo =<< get
  where 
    fsingle cvsInfo = do 
      xstate <- get 
      let hdlmodst = view hoodleModeState xstate
      let epage = getCurrentPageEitherFromHoodleModeState cvsInfo hdlmodst
          page = either id (hPage2RPage) epage 
          (_,currpage) = getCurrentLayerOrSet page
          Select (O (Just lyrzipper)) = view glayers currpage
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

