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
    commit . updatePageAll newxojstate . set xournalstate newxojstate $ xstate
   
    {-
    -- just for test
    xstate2 <- getSt
    let epg2 = get currentPage . getCurrentCanvasInfo $ xstate2
    liftIO $ putStrLn "after new page"
    liftIO $ either testPage (testPage . gcast) epg2 
    let currxoj = unView . get xournalstate $ xstate2       
        pagenum = get currentPageNum . getCurrentCanvasInfo $ xstate2 
    liftIO $ testPage (getPageFromGXournalMap pagenum currxoj)    -}

  where newlayeraction :: XournalState -> Int -> TPageBBoxMapPDFBuf -> MainCoroutine XournalState
        newlayeraction xojstate cpn page = do 
          let (_,currpage) = getCurrentLayerOrSet page
              Select (O (Just lyrzipper)) = get g_layers currpage  
          emptylyr <- liftIO emptyTLayerBBoxBufLyBuf 
          let nlyrzipper = appendGoLast lyrzipper emptylyr 
              npage = set g_layers (Select (O (Just nlyrzipper))) currpage
          --    (_,(l1,l2)) = unSZ nlyrzipper
          -- liftIO $ putStrLn . show $ (Seq.length l1, Seq.length l2)
          -- liftIO $ testPage npage
          return . setPageMap (M.adjust (const npage) cpn . getPageMap $ xojstate) $ xojstate  --  $ xojstate) $ xojstate

               
             
{-
testXournal :: XournalState -> IO () 
testXournal xojstate = do
  let xojsimple :: Xournal = case xojstate of
                               ViewAppendState xoj -> xournalFromTXournalSimple (gcast xoj :: TXournalSimple)
                               SelectState txoj -> xournalFromTXournalSimple (gcast txoj :: TXournalSimple)
  L.putStrLn (builder xojsimple)
-}


gotoNextLayer :: MainCoroutine ()
gotoNextLayer = do 
  liftIO $ putStrLn "gotoNextLayer called"

gotoPrevLayer :: MainCoroutine ()
gotoPrevLayer = do 
  liftIO $ putStrLn "gotoPrevLayer called"


deleteCurrentLayer :: MainCoroutine ()
deleteCurrentLayer = do 
  liftIO $ putStrLn "deleteCurrentLayer called"