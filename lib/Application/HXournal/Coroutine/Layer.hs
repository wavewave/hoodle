{-# LANGUAGE ScopedTypeVariables #-}

module Application.HXournal.Coroutine.Layer where

import Application.HXournal.Type.Canvas
import Application.HXournal.Type.XournalState
import Application.HXournal.Type.Coroutine
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

-- (.:) = (.).(.)

-- infixr 8 .: 

import qualified Data.Sequence as Seq

makeNewLayer :: MainCoroutine () 
makeNewLayer = do 
    liftIO $ putStrLn "makeNewLayer called"
    xstate <- getSt
    let epage = get currentPage . getCurrentCanvasInfo $ xstate
        cpn = get currentPageNum . getCurrentCanvasInfo $ xstate
        xojstate = get xournalstate xstate
    newxojstate <- either (newlayeraction xojstate cpn) (newlayeraction xojstate cpn . gcast) epage 
    commit . updatePageAll newxojstate $ xstate
  where newlayeraction :: XournalState -> Int -> TPageBBoxMapPDFBuf -> MainCoroutine XournalState
        newlayeraction xojstate cpn page = do 
          let (_,currpage) = getCurrentLayerOrSet page
              Select (O (Just lyrzipper)) = get g_layers currpage  
          emptylyr <- liftIO emptyTLayerBBoxBufLyBuf 
          let nlyrzipper = appendGoLast lyrzipper emptylyr 
              npage = set g_layers (Select (O (Just nlyrzipper))) currpage
              (_,(l1,l2)) = unSZ lyrzipper
          liftIO $ putStrLn . show $ (Seq.length l1, Seq.length l2)
          return . setPageMap (M.adjust (const npage) cpn . getPageMap $ xojstate) $ xojstate  --  $ xojstate) $ xojstate
             
gotoNextLayer :: MainCoroutine ()
gotoNextLayer = do 
  liftIO $ putStrLn "gotoNextLayer called"

gotoPrevLayer :: MainCoroutine ()
gotoPrevLayer = do 
  liftIO $ putStrLn "gotoPrevLayer called"


deleteCurrentLayer :: MainCoroutine ()
deleteCurrentLayer = do 
  liftIO $ putStrLn "deleteCurrentLayer called"