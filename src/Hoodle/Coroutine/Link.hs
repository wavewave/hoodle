{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Link
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Link where

import           Control.Applicative
import           Control.Lens (view,set,over,(%~))
import           Control.Monad.State 
import           Control.Monad.Trans.Either 
import           Control.Monad.Trans.Maybe 
import qualified Data.ByteString.Char8 as B 
import qualified Data.IntMap as M
import           Data.UUID.V4 (nextRandom)
import           Graphics.UI.Gtk hiding (get,set) 
import           System.FilePath 
-- from hoodle-platform
import           Control.Monad.Trans.Crtn
import           Control.Monad.Trans.Crtn.Event 
import           Control.Monad.Trans.Crtn.Queue 
import           Data.Hoodle.BBox
import           Data.Hoodle.Generic
import           Data.Hoodle.Select
import           Graphics.Hoodle.Render
import           Graphics.Hoodle.Render.Item 
import           Graphics.Hoodle.Render.Type 
import           Graphics.Hoodle.Render.Type.HitTest 
import           Graphics.Hoodle.Render.Util.HitTest 
-- from this package
import           Hoodle.Accessor
import           Hoodle.Coroutine.Draw
import           Hoodle.Coroutine.File 
import           Hoodle.Coroutine.Mode 
import           Hoodle.Coroutine.TextInput 
import           Hoodle.Device 
import           Hoodle.ModelAction.Layer 
import           Hoodle.ModelAction.Select
import           Hoodle.Type.Alias
import           Hoodle.Type.Canvas
import           Hoodle.Type.Coroutine
-- import           Hoodle.Type.Enum 
import           Hoodle.Type.Event
import           Hoodle.Type.HoodleState
import           Hoodle.Type.PageArrangement
import           Hoodle.Util 
import           Hoodle.View.Coordinate
import           Hoodle.View.Draw
--
import Prelude hiding (mapM_, mapM)

notifyLink :: CanvasId -> PointerCoord -> MainCoroutine () 
notifyLink cid pcoord = do 
    xst <- get 
    (boxAction f . getCanvasInfo cid) xst 
  where 
    f :: forall b. (ViewMode b) => CanvasInfo b -> MainCoroutine () 
    f cvsInfo = do 
      let cpn = PageNum . view currentPageNum $ cvsInfo
          arr = view (viewInfo.pageArrangement) cvsInfo              
          canvas = view drawArea cvsInfo
      geometry <- liftIO $ makeCanvasGeometry cpn arr canvas
      case (desktop2Page geometry . device2Desktop geometry) pcoord of
        Nothing -> return () 
        Just (pnum,PageCoord (x,y)) -> do 
          itms <- rItmsInCurrLyr    
          let lnks = filter isLinkInRItem itms           
              hlnks = hltFilteredBy (\itm->isPointInBBox (getBBox itm) (x,y)) lnks
              hitted = takeHitted hlnks 
          when ((not.null) hitted) $ do  
            let lnk = head hitted 
                bbx = getBBox lnk
                bbx_desk = xformBBox (unDeskCoord . page2Desktop geometry
                                      . (pnum,) . PageCoord) bbx
            invalidateInBBox (Just bbx_desk) Efficient cid 

-- | got a link address (or embedded image) from drag and drop             
gotLink :: Maybe String -> (Int,Int) -> MainCoroutine () 
gotLink mstr (x,y) = do 
  xst <- get 
  let cid = getCurrentCanvasId xst
  mr <- runMaybeT $ do 
    str <- (MaybeT . return) mstr 
    let (str1,rem1) = break (== ',') str 
    guard ((not.null) rem1)
    return (B.pack str1,tail rem1) 
  case mr of 
    Nothing -> do 
      mr2 <- runMaybeT $ do 
        str <- (MaybeT . return) mstr 
        (MaybeT . return) (urlParse str)
      case mr2 of  
        Nothing -> liftIO $ putStrLn "nothing" 
        Just (FileUrl file) -> do 
          liftIO $ print file 
          let ext = takeExtension file 
          if ext == ".png" || ext == ".PNG" || ext == ".jpg" || ext == ".JPG" 
            then do 
              let isembedded = view (settings.doesEmbedImage) xst 
              nitm <- liftIO (cnstrctRItem =<< makeNewItemImage isembedded file) 
              geometry <- liftIO $ getCanvasGeometryCvsId cid xst               
              let ccoord = CvsCoord (fromIntegral x,fromIntegral y)
                  mpgcoord = (desktop2Page geometry . canvas2Desktop geometry) 
                               ccoord 
              
              insertItemAt mpgcoord nitm 
            
              
{-              let ccoord = CvsCoord (fromIntegral x,fromIntegral y)
                  mpgcoord = (desktop2Page geometry . canvas2Desktop geometry) ccoord 
                  rdr' = case mpgcoord of 
                           Nothing -> rdr 
                           Just (_,PageCoord (x',y')) -> 
                             let bbox' = moveBBoxULCornerTo (x',y') (snd rdr) 
                             in (fst rdr,bbox')
                  
              liftIO $ print mpgcoord 
              liftIO $ print (snd rdr')
              linkInsert "simple" (uuidbstr,fp) fn rdr'  -}

            
            else return () 

         
      
      
    Just (uuidbstr,fp) -> do 
      let fn = takeFileName fp 
      rdr <- liftIO (makePangoTextSVG fn) 
      geometry <- liftIO $ getCanvasGeometryCvsId cid xst 
      let ccoord = CvsCoord (fromIntegral x,fromIntegral y)
          mpgcoord = (desktop2Page geometry . canvas2Desktop geometry) ccoord 
          rdr' = case mpgcoord of 
                   Nothing -> rdr 
                   Just (_,PageCoord (x',y')) -> 
                     let bbox' = moveBBoxULCornerTo (x',y') (snd rdr) 
                     in (fst rdr,bbox')
      liftIO $ print mpgcoord 
      liftIO $ print (snd rdr')
      linkInsert "simple" (uuidbstr,fp) fn rdr' 
  liftIO $ putStrLn "gotLink"
  liftIO $ print mstr 
  liftIO $ print (x,y)

-- | 
addLink :: MainCoroutine ()
addLink = do 
    mfilename <- fileChooser FileChooserActionOpen Nothing 
    modify (tempQueue %~ enqueue (action mfilename)) 
    minput <- go
    case minput of 
      Nothing -> return () 
      Just (str,fname) -> do 
        uuid <- liftIO $ nextRandom
        let uuidbstr = B.pack (show uuid)
        rdr <- liftIO (makePangoTextSVG str) 
        linkInsert "simple" (uuidbstr,fname) str rdr 
  where 
    go = do r <- nextevent
            case r of 
              AddLink minput -> return minput 
              UpdateCanvas cid -> -- this is temporary 
                                  (invalidateInBBox Nothing Efficient cid) >> go 
              _ -> go 
    action mfn = Left . ActionOrder $ 
                   \_evhandler -> do 
                     dialog <- messageDialogNew Nothing [DialogModal]
                                 MessageQuestion ButtonsOkCancel "add link" 
                     vbox <- dialogGetUpper dialog
                     txtvw <- textViewNew
                     boxPackStart vbox txtvw PackGrow 0 
                     widgetShowAll dialog
                     res <- dialogRun dialog 
                     case res of 
                       ResponseOk -> do 
                         buf <- textViewGetBuffer txtvw 
                         (istart,iend) <- (,) <$> textBufferGetStartIter buf
                                              <*> textBufferGetEndIter buf
                         l <- textBufferGetText buf istart iend True
                         widgetDestroy dialog
                         return (AddLink ((l,) <$> mfn))
                       _ -> do 
                         widgetDestroy dialog
                         return (AddLink Nothing)

                
            




