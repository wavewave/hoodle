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

import qualified Data.Sequence as Seq

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
  where newlayeraction :: XournalState -> Int -> TPageBBoxMapPDFBuf -> MainCoroutine XournalState
        newlayeraction xojstate cpn page = do 
          let (_,currpage) = getCurrentLayerOrSet page
              Select (O (Just lyrzipper)) = get g_layers currpage  
          emptylyr <- liftIO emptyTLayerBBoxBufLyBuf 
          let nlyrzipper = appendGoLast lyrzipper emptylyr 
              npage = set g_layers (Select (O (Just nlyrzipper))) currpage
          return . setPageMap (M.adjust (const npage) cpn . getPageMap $ xojstate) $ xojstate  --  $ xojstate) $ xojstate
                

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

deleteCurrentLayer :: MainCoroutine ()
deleteCurrentLayer = layerAction deletelayeraction >>= commit
  where deletelayeraction :: XournalState -> Int -> TPageBBoxMapPDFBuf -> MainCoroutine XournalState
        deletelayeraction xojstate cpn page = do 
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

          --    (_,(l1,l2)) = unSZ nlyrzipper
          -- liftIO $ putStrLn . show $ (Seq.length l1, Seq.length l2)
          -- liftIO $ testPage npage

    {-
    -- just for test
    xstate2 <- getSt
    let epg2 = get currentPage . getCurrentCanvasInfo $ xstate2
    liftIO $ putStrLn "after new page"
    liftIO $ either testPage (testPage . gcast) epg2 
    let currxoj = unView . get xournalstate $ xstate2       
        pagenum = get currentPageNum . getCurrentCanvasInfo $ xstate2 
    liftIO $ testPage (getPageFromGXournalMap pagenum currxoj)    -}

{-    liftIO $ putStrLn "makeNewLayer called"
    xstate <- getSt
    let epage = get currentPage . getCurrentCanvasInfo $ xstate
        cpn = get currentPageNum . getCurrentCanvasInfo $ xstate
        xojstate = get xournalstate xstate
    newxojstate <- either (newlayeraction xojstate cpn) (newlayeraction xojstate cpn . gcast) epage 
    commit . updatePageAll newxojstate . set xournalstate newxojstate $ xstate -}


{-
testXournal :: XournalState -> IO () 
testXournal xojstate = do
  let xojsimple :: Xournal = case xojstate of
                               ViewAppendState xoj -> xournalFromTXournalSimple (gcast xoj :: TXournalSimple)
                               SelectState txoj -> xournalFromTXournalSimple (gcast txoj :: TXournalSimple)
  L.putStrLn (builder xojsimple)
-}
