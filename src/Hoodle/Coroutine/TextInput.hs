{-# LANGUAGE OverloadedStrings, TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.TextInput 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.TextInput where

import           Control.Applicative
import           Control.Lens (view,set,(%~))
import           Control.Monad.State 
import           Control.Monad.Trans.Either
import           Data.Attoparsec
import           Graphics.Rendering.Cairo
import           Graphics.Rendering.Pango.Cairo
import           Graphics.UI.Gtk hiding (get,set)
-- 
import           Control.Monad.Trans.Crtn
import           Control.Monad.Trans.Crtn.Event 
import           Control.Monad.Trans.Crtn.Queue 
import qualified Data.ByteString.Char8 as B 
import           Data.Hoodle.BBox
import           Data.Hoodle.Generic
import           Data.Hoodle.Simple 
import           Graphics.Hoodle.Render.Item 
import           Graphics.Hoodle.Render.Type.HitTest
import           System.Directory 
import           System.FilePath 
import qualified Text.Hoodle.Parse.Attoparsec as PA
--
import           Hoodle.ModelAction.Layer 
import           Hoodle.ModelAction.Page
import           Hoodle.ModelAction.Select
import           Hoodle.Coroutine.Draw 
import           Hoodle.Coroutine.Mode
import           Hoodle.Type.Canvas 
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Event 
import           Hoodle.Type.HoodleState 
import           Hoodle.Util
-- 
import Prelude hiding (readFile)

-- | 
textInput :: MainCoroutine ()
textInput = do 
    modify (tempQueue %~ enqueue action) 
    minput <- go
    case minput of 
      Nothing -> return () 
      Just str -> liftIO (makePangoTextSVG str) >>= svgInsert str 
  where 
    go = do r <- nextevent
            case r of 
              TextInput input -> return input 
              _ -> go 
    action = Left . ActionOrder $ 
               \_evhandler -> do 
                 dialog <- messageDialogNew Nothing [DialogModal]
                   MessageQuestion ButtonsOkCancel "text input"
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
                     return (TextInput (Just l))
                   _ -> do 
                     widgetDestroy dialog
                     return (TextInput Nothing)

-- |
svgInsert :: String -> (B.ByteString,BBox) -> MainCoroutine () 
svgInsert str (svgbstr,BBox (x0,y0) (x1,y1)) = do 
    xstate <- get 
    let pgnum = unboxGet currentPageNum . view currentCanvasInfo $ xstate
        hdl = getHoodle xstate 
        currpage = getPageFromGHoodleMap pgnum hdl
        currlayer = getCurrentLayer currpage
  
    newitem <- (liftIO . cnstrctRItem . ItemSVG) 
                 (SVG (Just (B.pack str)) Nothing svgbstr 
                      (100,100) (Dim (x1-x0) (y1-y0)))  
    let otheritems = view gitems currlayer  
    let ntpg = makePageSelectMode currpage 
                 (otheritems :- (Hitted [newitem]) :- Empty)  
    modeChange ToSelectMode 
    nxstate <- get 
    thdl <- case view hoodleModeState nxstate of
              SelectState thdl' -> return thdl'
              _ -> (lift . EitherT . return . Left . Other) "svgInsert"
    nthdl <- liftIO $ updateTempHoodleSelectIO thdl ntpg pgnum 
    let nxstate2 = set hoodleModeState (SelectState nthdl) nxstate
    put nxstate2
    invalidateAll 
  
-- |     
convertLinkFromSimpleToDocID :: Link -> IO (Maybe Link)
convertLinkFromSimpleToDocID (Link i _typ lstr txt cmd rdr pos dim) = do 
    case urlParse (B.unpack lstr) of 
      Nothing -> return Nothing
      Just (HttpUrl url) -> return Nothing
      Just (FileUrl file) -> do 
        b <- doesFileExist file
        if b 
          then do 
            bstr <- B.readFile file
            case parseOnly PA.hoodle bstr of 
              Left str -> return Nothing
              Right hdl -> do 
                let uuid = view hoodleID hdl
                    link = LinkDocID i uuid (B.pack file) txt cmd rdr pos dim
                return (Just link)
          else return Nothing 
convertLinkFromSimpleToDocID _ = return Nothing 
    
    
-- |   
linkInsert :: B.ByteString 
              -> (B.ByteString,FilePath)
              -> String 
              -> (B.ByteString,BBox) 
              -> MainCoroutine ()
linkInsert typ (uuidbstr,fname) str (svgbstr,BBox (x0,y0) (x1,y1)) = do 
    xstate <- get 
    let pgnum = unboxGet currentPageNum . view currentCanvasInfo $ xstate
        hdl = getHoodle xstate 
        currpage = getPageFromGHoodleMap pgnum hdl
        currlayer = getCurrentLayer currpage
        lnk = Link uuidbstr "simple" (B.pack fname) (Just (B.pack str)) Nothing svgbstr 
                  (x0,y0) (Dim (x1-x0) (y1-y0))
    nlnk <- liftIO $ convertLinkFromSimpleToDocID lnk >>= maybe (return lnk) return
    liftIO $ print nlnk
    newitem <- (liftIO . cnstrctRItem . ItemLink) nlnk
    let otheritems = view gitems currlayer  
    let ntpg = makePageSelectMode currpage 
                 (otheritems :- (Hitted [newitem]) :- Empty)  
    modeChange ToSelectMode 
    nxstate <- get 
    thdl <- case view hoodleModeState nxstate of
              SelectState thdl' -> return thdl'
              _ -> (lift . EitherT . return . Left . Other) "linkInsert"
    nthdl <- liftIO $ updateTempHoodleSelectIO thdl ntpg pgnum 
    let nxstate2 = set hoodleModeState (SelectState nthdl) nxstate
    put nxstate2
    invalidateAll 

-- |
makePangoTextSVG :: String -> IO (B.ByteString,BBox) 
makePangoTextSVG str = do 
    let pangordr = do 
          ctxt <- cairoCreateContext Nothing 
          layout <- layoutEmpty ctxt   
          layoutSetWidth layout (Just 400)
          layoutSetWrap layout WrapAnywhere 
          layoutSetText layout str 
          (_,reclog) <- layoutGetExtents layout 
          let PangoRectangle x y w h = reclog 
          -- 10 is just dirty-fix
          return (layout,BBox (x,y) (x+w+10,y+h)) 
        rdr layout = do setSourceRGBA 0 0 0 1
                        updateLayout layout 
                        showLayout layout 
    (layout,(BBox (x0,y0) (x1,y1))) <- pangordr 
    tdir <- getTemporaryDirectory 
    let tfile = tdir </> "embedded.svg"
    withSVGSurface tfile (x1-x0) (y1-y0) $ \s -> renderWith s (rdr layout)
    bstr <- B.readFile tfile 
    return (bstr,BBox (x0,y0) (x1,y1)) 


