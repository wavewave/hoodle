{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.ModelAction.ContextMenu
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.ModelAction.ContextMenu where

import           Control.Concurrent (forkIO, threadDelay)
import qualified Data.ByteString.Char8 as B
import           Data.Foldable (forM_)
import           Data.UUID.V4
import           DBus 
import           DBus.Client 
import qualified Graphics.Rendering.Cairo as Cairo
import           Graphics.UI.Gtk
import           System.Directory 
import           System.FilePath 
import           System.Process
-- 
import           Data.Hoodle.BBox
import           Data.Hoodle.Simple
import           Graphics.Hoodle.Render 
import           Graphics.Hoodle.Render.Type
--
import Hoodle.Type.Event 
import Hoodle.Util

-- |
menuOpenALink :: UrlPath -> IO MenuItem
menuOpenALink urlpath = do 
    let urlname = case urlpath of 
                    FileUrl fp -> fp 
                    HttpUrl url -> url 
    menuitemlnk <- menuItemNewWithLabel ("Open "++urlname) 
    menuitemlnk `on` menuItemActivate $ openLinkAction urlpath Nothing
    return menuitemlnk


-- | 
openLinkAction :: UrlPath 
               -> Maybe (B.ByteString,B.ByteString) -- ^ (docid,anchorid)
               -> IO () 
openLinkAction urlpath mid = do
    cli <- connectSession
    case urlpath of 
      FileUrl fp -> do 
        putStrLn "test dbus"
        emit cli (signal "/" "org.ianwookim.hoodle" "findWindow") 
                   { signalBody = [ toVariant fp] }         
        return () 
      HttpUrl url -> do 
        let cmdargs = [url]
        createProcess (proc "xdg-open" cmdargs)  
        return () 
    forkIO $ do 
      threadDelay 2000000
      forM_ mid $ \(docid,anchorid) -> do
                  print (docid,anchorid)
                  emit cli (signal "/" "org.ianwookim.hoodle" "callLink")
                             { signalBody = 
                                 [ toVariant (B.unpack docid 
                                              ++ "," 
                                              ++ B.unpack anchorid) ] }
    return ()
-- | 
menuCreateALink :: (AllEvent -> IO ()) -> [RItem] -> IO (Maybe MenuItem)
menuCreateALink evhandler sitems = 
  if (length . filter isLinkInRItem) sitems > 0
  then return Nothing 
  else do mi <- menuItemNewWithLabel "Create a link to..." 
          mi `on` menuItemActivate $ 
            evhandler (UsrEv (GotContextMenuSignal CMenuCreateALink))
          return (Just mi)
         

-- |
makeSVGFromSelection :: RenderCache -> [RItem] -> BBox -> IO SVG 
makeSVGFromSelection cache hititms (BBox (ulx,uly) (lrx,lry)) = do 
  uuid <- nextRandom
  tdir <- getTemporaryDirectory
  let filename = tdir </> show uuid <.> "svg"
      (x,y) = (ulx,uly)
      (w,h) = (lrx-ulx,lry-uly)
  Cairo.withSVGSurface filename w h $ \s -> Cairo.renderWith s $ do 
    Cairo.translate (-ulx) (-uly) 
    mapM_ (renderRItem cache) hititms 
  bstr <- B.readFile filename
  let svg = SVG Nothing Nothing bstr (x,y) (Dim w h)
  svg `seq` removeFile filename 
  return svg                       

