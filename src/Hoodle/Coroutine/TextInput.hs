{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

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
import           Control.Monad.State hiding (mapM_, forM_)
import           Control.Monad.Trans.Either
import           Data.Attoparsec
import           Data.Foldable (mapM_, forM_)
import           Data.UUID.V4 (nextRandom)
import           Graphics.Rendering.Cairo
import qualified Graphics.Rendering.Cairo.SVG as RSVG
import           Graphics.Rendering.Pango.Cairo
import           Graphics.UI.Gtk hiding (get,set)
import           System.Directory 
import           System.Exit (ExitCode(..))
import           System.FilePath 
import           System.IO (readFile)
import           System.Process (system,readProcessWithExitCode)
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
import qualified Text.Hoodle.Parse.Attoparsec as PA
--
import           Hoodle.ModelAction.Layer 
import           Hoodle.ModelAction.Page
import           Hoodle.ModelAction.Select
import           Hoodle.Coroutine.Commit
import           Hoodle.Coroutine.Dialog
import           Hoodle.Coroutine.Draw 
import           Hoodle.Coroutine.Mode
import           Hoodle.Coroutine.Select.Clipboard
import           Hoodle.Type.Canvas 
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Enum
import           Hoodle.Type.Event 
import           Hoodle.Type.HoodleState 
import           Hoodle.Util
-- 
import Prelude hiding (readFile,mapM_,forM_)

multiLineDialog :: String -> Either (ActionOrder AllEvent) AllEvent
multiLineDialog str = mkIOaction $ \evhandler -> do
    dialog <- dialogNew
    vbox <- dialogGetUpper dialog
    textbuf <- textBufferNew Nothing
    textBufferSetText textbuf str
    textbuf `on` bufferChanged $ do 
        (s,e) <- (,) <$> textBufferGetStartIter textbuf <*> textBufferGetEndIter textbuf  
        contents <- textBufferGetText textbuf s e False
        (evhandler . UsrEv . MultiLine . MultiLineChanged) contents
    textarea <- textViewNewWithBuffer textbuf
    vscrbar <- vScrollbarNew =<< textViewGetVadjustment textarea
    hscrbar <- hScrollbarNew =<< textViewGetHadjustment textarea 
    textarea `on` sizeRequest $ return (Requisition 500 600)
    fdesc <- fontDescriptionNew
    fontDescriptionSetFamily fdesc "Mono"
    widgetModifyFont textarea (Just fdesc)
    -- 
    table <- tableNew 2 2 False
    tableAttachDefaults table textarea 0 1 0 1
    tableAttachDefaults table vscrbar 1 2 0 1
    tableAttachDefaults table hscrbar 0 1 1 2 
    boxPackStart vbox table PackNatural 0
    -- 
    btnOk <- dialogAddButton dialog "Ok" ResponseOk
    btnCancel <- dialogAddButton dialog "Cancel" ResponseCancel
    widgetShowAll dialog
    res <- dialogRun dialog
    widgetDestroy dialog
    case res of 
      ResponseOk -> return (UsrEv (OkCancel True))
      ResponseCancel -> return (UsrEv (OkCancel False))
      _ -> return (UsrEv (OkCancel False))

multiLineLoop :: String -> MainCoroutine (Maybe String)
multiLineLoop str = do 
    r <- nextevent
    case r of 
      UpdateCanvas cid -> invalidateInBBox Nothing Efficient cid 
                          >> multiLineLoop str
      OkCancel True -> (return . Just) str
      OkCancel False -> return Nothing
      MultiLine (MultiLineChanged str') -> multiLineLoop str'
      _ -> multiLineLoop str

-- | 
textInputDialog :: MainCoroutine (Maybe String) 
textInputDialog = do 
  doIOaction $ \_evhandler -> do 
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
                     return (UsrEv (TextInput (Just l)))
                   _ -> do 
                     widgetDestroy dialog
                     return (UsrEv (TextInput Nothing))
  let go = do r <- nextevent
              case r of 
                TextInput input -> return input 
                UpdateCanvas cid -> invalidateInBBox Nothing Efficient cid >> go 
                _ -> go 
  go 

-- | insert text 
textInput :: String -> MainCoroutine ()
textInput str = do 
    modify (tempQueue %~ enqueue (multiLineDialog str))  
    multiLineLoop str >>= 
      mapM_ (\result -> deleteSelection
                        >> liftIO (makePangoTextSVG result) 
                        >>= svgInsert (result,"pango"))
  
-- | insert latex
laTeXInput :: String -> MainCoroutine ()
laTeXInput str = do 
    modify (tempQueue %~ enqueue (multiLineDialog str))  
    multiLineLoop str >>= 
      mapM_ (\result -> deleteSelection
                        >> liftIO (makeLaTeXSVG result) 
                        >>= \case Right r -> svgInsert (result,"latex") r
                                  Left err -> okMessageBox err
                                              -- do liftIO (putStrLn err)
            )
      
laTeXHeader :: String
laTeXHeader = "\\documentclass{article}\n\
              \\\pagestyle{empty}\n\
              \\\begin{document}\n"
                                
laTeXFooter :: String
laTeXFooter = "\\end{document}\n"

makeLaTeXSVG :: String -> IO (Either String (B.ByteString,BBox))
makeLaTeXSVG txt = do
    cdir <- getCurrentDirectory
    tdir <- getTemporaryDirectory
    tfilename <- show <$> nextRandom
    
    setCurrentDirectory tdir
    let check msg act = liftIO act >>= \case ExitSuccess -> right () ; _ -> left msg
        
    writeFile (tfilename <.> "tex") txt
    r <- runEitherT $ do 
      check "latex" $ do (ecode,ostr,estr) <- readProcessWithExitCode "pdflatex" [tfilename <.> "tex"] ""
                         return ecode
      check "crop" (system ("pdfcrop " ++ tfilename <.> "pdf" ++ " " ++ tfilename ++ "_crop" <.> "pdf"))
      check "svg" (system ("pdf2svg " ++ tfilename ++ "_crop" <.> "pdf" ++ " " ++ tfilename <.> "svg"))
      bstr <- liftIO $ B.readFile (tfilename <.> "svg")
      rsvg <- liftIO $ RSVG.svgNewFromString (B.unpack bstr) 
      let (w,h) = RSVG.svgGetSize rsvg
      return (bstr,BBox (100,100) (100+fromIntegral w,100+fromIntegral h)) 
    setCurrentDirectory cdir
    return r
    

-- |
svgInsert :: (String,String) -> (B.ByteString,BBox) -> MainCoroutine () 
svgInsert (str,cmd) (svgbstr,BBox (x0,y0) (x1,y1)) = do 
    xstate <- get 
    let pgnum = view (unboxLens currentPageNum) . view currentCanvasInfo $ xstate
        hdl = getHoodle xstate 
        currpage = getPageFromGHoodleMap pgnum hdl
        currlayer = getCurrentLayer currpage
    newitem <- (liftIO . cnstrctRItem . ItemSVG) 
                 (SVG (Just (B.pack str)) (Just (B.pack cmd)) svgbstr 
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
    put (set hoodleModeState (SelectState nthdl) nxstate)
    commit_
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
    let pgnum = view (currentCanvasInfo . unboxLens currentPageNum) xstate
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


