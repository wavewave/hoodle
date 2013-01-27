{-# LANGUAGE OverloadedStrings #-}

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
import           Control.Lens
import           Control.Monad.State 
-- import           Control.Monad.Trans
import           Control.Monad.Trans.Either
import           Graphics.Rendering.Cairo
import           Graphics.Rendering.Pango.Cairo
import           Graphics.UI.Gtk hiding (get,set)
-- 
import           Control.Monad.Trans.Crtn
import           Control.Monad.Trans.Crtn.Event 
import           Control.Monad.Trans.Crtn.Queue 
import           Data.ByteString (readFile)
import qualified Data.ByteString.Char8 as B (pack)
import           Data.Hoodle.BBox
import           Data.Hoodle.Generic
import           Data.Hoodle.Simple 
import           Graphics.Hoodle.Render.Item 
-- import           Graphics.Hoodle.Render.Type
import           Graphics.Hoodle.Render.Type.HitTest
import           System.Directory 
-- import           System.Environment
-- import           System.Exit 
import           System.FilePath 
-- import           System.Process 
--
-- import           Hoodle.Accessor
import           Hoodle.ModelAction.Layer 
import           Hoodle.ModelAction.Page
import           Hoodle.ModelAction.Select
import           Hoodle.Coroutine.Draw 
import           Hoodle.Coroutine.Mode
import           Hoodle.Type.Canvas 
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Event hiding (SVG)
import           Hoodle.Type.HoodleState 
import           Hoodle.Util
-- 
import Prelude hiding (readFile)


textInput :: MainCoroutine ()
textInput = do 
    liftIO $ putStrLn "textInput"
    modify (tempQueue %~ enqueue action) 
    minput <- go
    case minput of 
      Nothing -> return () 
      Just str -> makePangoTextSVGInsert str  
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

makePangoTextSVGInsert :: String -> MainCoroutine () 
makePangoTextSVGInsert str = do 
    xstate <- get 
    liftIO $ putStrLn str 
    let pgnum = unboxGet currentPageNum . view currentCanvasInfo $ xstate
        hdl = getHoodle xstate 
        currpage = getPageFromGHoodleMap pgnum hdl
        currlayer = getCurrentLayer currpage
        pangordr = do 
          ctxt <- cairoCreateContext Nothing 
          layout <- layoutEmpty ctxt   
          layoutSetWidth layout (Just 300)
          layoutSetWrap layout WrapAnywhere 
          layoutSetText layout str 
          (_,reclog) <- layoutGetExtents layout 
          let PangoRectangle x y w h = reclog 
          return (layout,BBox (x,y) (x+w,y+h)) 
        rdr layout = do setSourceRGBA 0 0 0 1
                        -- layout <- createLayout str
                        -- liftIO $ layoutSetWidth layout (Just 300)
                        -- liftIO $ layoutSetWrap layout WrapAnywhere 
                        updateLayout layout 
                        showLayout layout 
    (layout,BBox (x0,y0) (x1,y1)) <- liftIO pangordr 
    
    tdir <- liftIO $ getTemporaryDirectory 
    let tfile = tdir </> "embedded.svg"
    liftIO $ withSVGSurface tfile (x1-x0) (y1-y0) $ \s -> renderWith s (rdr layout)
    svg <- liftIO $ readFile tfile 
    newitem <- (liftIO . cnstrctRItem . ItemSVG) 
                 (SVG (Just (B.pack str)) Nothing svg (100,100) (Dim (x1-x0) (y1-y0)))  
    let otheritems = view gitems currlayer  
    let ntpg = makePageSelectMode currpage (otheritems :- (Hitted [newitem]) :- Empty)  
    modeChange ToSelectMode 
    nxstate <- get 
    thdl <- case view hoodleModeState nxstate of
              SelectState thdl' -> return thdl'
              _ -> (lift . EitherT . return . Left . Other) "makePangoTextSVGInsert"
    nthdl <- liftIO $ updateTempHoodleSelectIO thdl ntpg pgnum 
    let nxstate2 = set hoodleModeState (SelectState nthdl) nxstate
    put nxstate2
    invalidateAll 


{-                     tdir <- getTemporaryDirectory
                     writeFile (tdir </> "latextest.tex") l 
                     let cmd = "lasem-render-0.6 " ++ (tdir </> "latextest.tex") ++ " -f svg -o " ++ (tdir </> "latextest.svg" )
                     print cmd 
                     excode <- system cmd 
                     case excode of 
                       ExitSuccess -> do 
                         svg <- readFile (tdir </> "latextest.svg")
                         return (LaTeXInput (Just (B.pack l,svg)))
                       _ -> return (LaTeXInput Nothing) -}

