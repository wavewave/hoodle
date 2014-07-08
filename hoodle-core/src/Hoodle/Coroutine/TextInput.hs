{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.TextInput 
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.TextInput where

import           Control.Applicative
-- import           Control.Concurrent.STM (atomically, newTVar)
import           Control.Lens (_1,_2,_3,view,set,(%~),(^.),(.~))
import           Control.Monad.State hiding (mapM_, forM_)
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Maybe
-- import           Data.Attoparsec
import           Data.Attoparsec.Char8
import qualified Data.ByteString.Char8 as B 
import           Data.Foldable (mapM_, forM_)
import           Data.List (sortBy)
import qualified Data.Map as M
import           Data.Maybe (catMaybes)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import           Data.UUID.V4 (nextRandom)
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Cairo.SVG as RSVG
-- import qualified Graphics.Rendering.Pango.Cairo as Pango
import qualified Graphics.UI.Gtk as Gtk
import           System.Directory 
import           System.Exit (ExitCode(..))
import           System.FilePath 
import           System.Process (readProcessWithExitCode)
-- 
import           Control.Monad.Trans.Crtn
import           Control.Monad.Trans.Crtn.Event 
import           Control.Monad.Trans.Crtn.Queue 
import           Data.Hoodle.BBox
import           Data.Hoodle.Generic
import           Data.Hoodle.Select
import           Data.Hoodle.Simple
import           Graphics.Hoodle.Render.Item
import           Graphics.Hoodle.Render.Type.HitTest
import           Graphics.Hoodle.Render.Type.Hoodle (rHoodle2Hoodle, rPage2Page)
import           Graphics.Hoodle.Render.Type.Item
import qualified Text.Hoodle.Parse.Attoparsec as PA
--
import           Hoodle.Accessor
import           Hoodle.Coroutine.Commit
import           Hoodle.Coroutine.Dialog
import           Hoodle.Coroutine.Draw 
import           Hoodle.Coroutine.Mode
import           Hoodle.Coroutine.Network
import           Hoodle.Coroutine.Select.Clipboard
import           Hoodle.ModelAction.Layer 
import           Hoodle.ModelAction.Page
import           Hoodle.ModelAction.Select
import           Hoodle.ModelAction.Select.Transform
import           Hoodle.ModelAction.Text
import           Hoodle.Type.Canvas 
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Enum
import           Hoodle.Type.Event 
import           Hoodle.Type.HoodleState 
import           Hoodle.Type.PageArrangement
import           Hoodle.Util
-- 
import Prelude hiding (readFile,mapM_)


-- | single line text input : almost abandoned now
textInputDialog :: MainCoroutine (Maybe String) 
textInputDialog = do 
  doIOaction $ \_evhandler -> do 
                 dialog <- Gtk.messageDialogNew Nothing [Gtk.DialogModal]
                   Gtk.MessageQuestion Gtk.ButtonsOkCancel "text input"
                 vbox <- Gtk.dialogGetUpper dialog
                 txtvw <- Gtk.textViewNew
                 Gtk.boxPackStart vbox txtvw Gtk.PackGrow 0 
                 Gtk.widgetShowAll dialog
                 res <- Gtk.dialogRun dialog 
                 case res of 
                   Gtk.ResponseOk -> do 
                     buf <- Gtk.textViewGetBuffer txtvw 
                     (istart,iend) <- (,) <$> Gtk.textBufferGetStartIter buf
                                          <*> Gtk.textBufferGetEndIter buf
                     l <- Gtk.textBufferGetText buf istart iend True
                     Gtk.widgetDestroy dialog
                     return (UsrEv (TextInput (Just l)))
                   _ -> do 
                     Gtk.widgetDestroy dialog
                     return (UsrEv (TextInput Nothing))
  let go = do r <- nextevent
              case r of 
                TextInput input -> return input 
                UpdateCanvas cid -> invalidateInBBox Nothing Efficient cid >> go 
                _ -> go 
  go 

-- | common dialog with multiline edit input box 
multiLineDialog :: T.Text -> Either (ActionOrder AllEvent) AllEvent
multiLineDialog str = mkIOaction $ \evhandler -> do
    dialog <- Gtk.dialogNew
    vbox <- Gtk.dialogGetUpper dialog
    textbuf <- Gtk.textBufferNew Nothing
    Gtk.textBufferSetByteString textbuf (TE.encodeUtf8 str)
    textbuf `Gtk.on` Gtk.bufferChanged $ do 
        (s,e) <- (,) <$> Gtk.textBufferGetStartIter textbuf <*> Gtk.textBufferGetEndIter textbuf
        contents <- Gtk.textBufferGetByteString textbuf s e False
        (evhandler . UsrEv . MultiLine . MultiLineChanged) (TE.decodeUtf8 contents)
    textarea <- Gtk.textViewNewWithBuffer textbuf
    vscrbar <- Gtk.vScrollbarNew =<< Gtk.textViewGetVadjustment textarea
    hscrbar <- Gtk.hScrollbarNew =<< Gtk.textViewGetHadjustment textarea 
    textarea `Gtk.on` Gtk.sizeRequest $ return (Gtk.Requisition 500 600)
    fdesc <- Gtk.fontDescriptionNew
    Gtk.fontDescriptionSetFamily fdesc "Mono"
    Gtk.widgetModifyFont textarea (Just fdesc)
    -- 
    table <- Gtk.tableNew 2 2 False
    Gtk.tableAttachDefaults table textarea 0 1 0 1
    Gtk.tableAttachDefaults table vscrbar 1 2 0 1
    Gtk.tableAttachDefaults table hscrbar 0 1 1 2 
    Gtk.boxPackStart vbox table Gtk.PackNatural 0
    -- 
    _btnOk <- Gtk.dialogAddButton dialog "Ok" Gtk.ResponseOk
    _btnCancel <- Gtk.dialogAddButton dialog "Cancel" Gtk.ResponseCancel
    _btnNetwork <- Gtk.dialogAddButton dialog "Network" (Gtk.ResponseUser 1)
    Gtk.widgetShowAll dialog
    res <- Gtk.dialogRun dialog
    Gtk.widgetDestroy dialog
    case res of 
      Gtk.ResponseOk -> return (UsrEv (OkCancel True))
      Gtk.ResponseCancel -> return (UsrEv (OkCancel False))
      Gtk.ResponseUser 1 -> return (UsrEv (NetworkProcess NetworkDialog))
      _ -> return (UsrEv (OkCancel False))

-- | main event loop for multiline edit box
multiLineLoop :: T.Text -> MainCoroutine (Maybe T.Text)
multiLineLoop txt = do 
    r <- nextevent
    case r of 
      UpdateCanvas cid -> invalidateInBBox Nothing Efficient cid 
                          >> multiLineLoop txt
      OkCancel True -> (return . Just) txt
      OkCancel False -> return Nothing
      NetworkProcess NetworkDialog -> networkTextInput txt
      MultiLine (MultiLineChanged txt') -> multiLineLoop txt'
      _ -> multiLineLoop txt

-- | insert text 
textInput :: Maybe (Double,Double) -> T.Text -> MainCoroutine ()
textInput mpos str = do 
    case mpos of 
      Just (x0,y0) -> do 
        modify (tempQueue %~ enqueue (multiLineDialog str))  
        multiLineLoop str >>= 
          mapM_ (\result -> deleteSelection
                            >> liftIO (makePangoTextSVG (x0,y0) result) 
                            >>= svgInsert (result,"pango"))
      Nothing -> liftIO $ putStrLn "textInput: not implemented"
  
-- | insert latex
laTeXInput :: Maybe (Double,Double) -> T.Text -> MainCoroutine ()
laTeXInput mpos str = do 
    case mpos of 
      Just (x0,y0) -> do 
        modify (tempQueue %~ enqueue (multiLineDialog str))  
        multiLineLoop str >>= 
          mapM_ (\result -> liftIO (makeLaTeXSVG (x0,y0) result) >>= \case 
                  Right r -> deleteSelection >> svgInsert (result,"latex") r
                  Left err -> okMessageBox err >> laTeXInput mpos result
                )
      Nothing -> do 
        modeChange ToViewAppendMode 
        autoPosText >>=
          maybe (laTeXInput (Just (100,100)) str) 
                (\y'->laTeXInput (Just (100,y')) str) 

autoPosText :: MainCoroutine (Maybe Double)
autoPosText = do 
    cpg <- rPage2Page <$> getCurrentPageCurr
    let Dim _pgw pgh = view dimension cpg
        mcomponents = do 
          l <- view layers cpg
          i <- view items l
          case i of 
            ItemSVG svg ->  
              case svg_command svg of
                Just "latex" -> do 
                  let (_,y) = svg_pos svg  
                      Dim _ h = svg_dim svg
                  return (y,y+h) 
                _ -> []
            ItemImage img -> do 
              let (_,y) = img_pos img
                  Dim _ h = img_dim img
              return (y,y+h)
                
            _ -> []
    if null mcomponents 
      then return Nothing 
      else do let y0 = (head . sortBy (flip compare) . map snd) mcomponents
              if y0 + 10 > pgh then return Nothing else return (Just (y0 + 10))


-- | 
laTeXInputNetwork :: Maybe (Double,Double) -> T.Text -> MainCoroutine ()
laTeXInputNetwork mpos str =  
    case mpos of 
      Just (x0,y0) -> do 
        networkTextInput str >>=
          mapM_ (\result -> liftIO (makeLaTeXSVG (x0,y0) result) 
                            >>= \case Right r -> deleteSelection >> svgInsert (result,"latex") r
                                      Left err -> okMessageBox err >> laTeXInput mpos result
                )
      Nothing -> do 
        modeChange ToViewAppendMode 
        autoPosText >>=
          maybe (laTeXInputNetwork (Just (100,100)) str) 
                (\y'->laTeXInputNetwork (Just (100,y')) str) 


dbusNetworkInput :: T.Text -> MainCoroutine ()
dbusNetworkInput txt = do 
    modeChange ToViewAppendMode     
    mpos <- autoPosText 
    let pos = maybe (100,100) (100,) mpos 
    rsvg <- liftIO (makeLaTeXSVG pos txt) 
    case rsvg of 
      Right r -> deleteSelection >> svgInsert (txt,"latex") r
      Left err -> okMessageBox err >> laTeXInput (Just pos) txt


laTeXHeader :: T.Text
laTeXHeader = "\\documentclass{article}\n\
              \\\pagestyle{empty}\n\
              \\\begin{document}\n"
                                
laTeXFooter :: T.Text
laTeXFooter = "\\end{document}\n"

makeLaTeXSVG :: (Double,Double) -> T.Text 
             -> IO (Either String (B.ByteString,BBox))
makeLaTeXSVG (x0,y0) txt = do
    cdir <- getCurrentDirectory
    tdir <- getTemporaryDirectory
    tfilename <- show <$> nextRandom
    
    setCurrentDirectory tdir
    let check msg act = liftIO act >>= \(ecode,str) -> case ecode of ExitSuccess -> right () ; _ -> left (msg ++ ":" ++ str)
        
    B.writeFile (tfilename <.> "tex") (TE.encodeUtf8 txt)
    r <- runEitherT $ do 
      check "error during xelatex" $ do 
        (ecode,ostr,estr) <- readProcessWithExitCode "xelatex" [tfilename <.> "tex"] ""
        return (ecode,ostr++estr)
      check "error during pdfcrop" $ do 
        (ecode,ostr,estr) <- readProcessWithExitCode "pdfcrop" [tfilename <.> "pdf",tfilename ++ "_crop" <.> "pdf"] ""       
        return (ecode,ostr++estr)
      check "error during pdf2svg" $ do
        (ecode,ostr,estr) <- readProcessWithExitCode "pdf2svg" [tfilename ++ "_crop" <.> "pdf",tfilename <.> "svg"] ""
        return (ecode,ostr++estr)
      bstr <- liftIO $ B.readFile (tfilename <.> "svg")
      rsvg <- liftIO $ RSVG.svgNewFromString (B.unpack bstr) 
      let (w,h) = RSVG.svgGetSize rsvg
      return (bstr,BBox (x0,y0) (x0+fromIntegral w,y0+fromIntegral h)) 
    setCurrentDirectory cdir
    return r
    

-- |
svgInsert :: (T.Text,String) -> (B.ByteString,BBox) -> MainCoroutine () 
svgInsert (txt,cmd) (svgbstr,BBox (x0,y0) (x1,y1)) = do 
    xstate <- get 
    let pgnum = view (unboxLens currentPageNum) . view currentCanvasInfo $ xstate
        hdl = getHoodle xstate 
        currpage = getPageFromGHoodleMap pgnum hdl
        currlayer = getCurrentLayer currpage
    --
    callRenderer ( (return . GotRItem) =<< 
      (cnstrctRItem (ItemSVG (SVG (Just (TE.encodeUtf8 txt)) (Just (B.pack cmd)) svgbstr (x0,y0) (Dim (x1-x0) (y1-y0))))))
    
    RenderEv (GotRItem newitem) <- 
      waitSomeEvent (\case RenderEv (GotRItem _) -> True ; _ -> False )
    --             
    let otheritems = view gitems currlayer  
    let ntpg = makePageSelectMode currpage 
                 (otheritems :- (Hitted [newitem]) :- Empty)  
        cache = view renderCache xstate
    modeChange ToSelectMode 
    nxstate <- get 
    thdl <- case view hoodleModeState nxstate of
              SelectState thdl' -> return thdl'
              _ -> (lift . EitherT . return . Left . Other) "svgInsert"
    nthdl <- liftIO $ updateTempHoodleSelectIO cache thdl ntpg pgnum 
    put (set hoodleModeState (SelectState nthdl) nxstate)
    commit_
    invalidateAll 
  
-- |     
convertLinkFromSimpleToDocID :: Link -> IO (Maybe Link)
convertLinkFromSimpleToDocID (Link i _typ lstr txt cmd rdr pos dim) = do 
    case urlParse (B.unpack lstr) of 
      Nothing -> return Nothing
      Just (HttpUrl _url) -> return Nothing
      Just (FileUrl file) -> do 
        b <- doesFileExist file
        if b 
          then do 
            bstr <- B.readFile file
            case parseOnly PA.hoodle bstr of 
              Left _str -> return Nothing
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
linkInsert _typ (uuidbstr,fname) str (svgbstr,BBox (x0,y0) (x1,y1)) = do 
    xstate <- get 
    let pgnum = view (currentCanvasInfo . unboxLens currentPageNum) xstate 
        lnk = Link uuidbstr "simple" (B.pack fname) (Just (B.pack str)) Nothing svgbstr 
                  (x0,y0) (Dim (x1-x0) (y1-y0))
    nlnk <- liftIO $ convertLinkFromSimpleToDocID lnk >>= maybe (return lnk) return
    --
    callRenderer $ return . GotRItem =<< cnstrctRItem (ItemLink nlnk) 
    RenderEv (GotRItem newitem) <- 
      waitSomeEvent (\case RenderEv (GotRItem _) -> True; _ -> False) 
    --
    insertItemAt (Just (PageNum pgnum, PageCoord (x0,y0))) newitem


-- | anchor 
addAnchor :: MainCoroutine ()
addAnchor = do
    uuid <- liftIO $ nextRandom
    let uuidbstr = B.pack (show uuid)
    let anc = Anchor uuidbstr "" (100,100) (Dim 50 50)
    --
    callRenderer $ return . GotRItem =<< cnstrctRItem (ItemAnchor anc)
    RenderEv (GotRItem nitm) <- 
      waitSomeEvent (\case RenderEv (GotRItem _) -> True; _ -> False)
    --
    insertItemAt Nothing nitm

-- |
makePangoTextSVG :: (Double,Double) -> T.Text -> IO (B.ByteString,BBox) 
makePangoTextSVG (xo,yo) str = do 
    let pangordr = do 
          ctxt <- Gtk.cairoCreateContext Nothing 
          layout <- Gtk.layoutEmpty ctxt   
          Gtk.layoutSetWidth layout (Just 400)
          Gtk.layoutSetWrap layout Gtk.WrapAnywhere 
          Gtk.layoutSetText layout (T.unpack str) -- this is gtk2hs pango limitation 
          (_,reclog) <- Gtk.layoutGetExtents layout 
          let Gtk.PangoRectangle x y w h = reclog 
          -- 10 is just dirty-fix
          return (layout,BBox (x,y) (x+w+10,y+h)) 
        rdr layout = do Cairo.setSourceRGBA 0 0 0 1
                        Gtk.updateLayout layout 
                        Gtk.showLayout layout 
    (layout,(BBox (x0,y0) (x1,y1))) <- pangordr 
    tdir <- getTemporaryDirectory 
    let tfile = tdir </> "embedded.svg"
    Cairo.withSVGSurface tfile (x1-x0) (y1-y0) $ \s -> 
      Cairo.renderWith s (rdr layout)
    bstr <- B.readFile tfile 
    return (bstr,BBox (xo,yo) (xo+x1-x0,yo+y1-y0)) 

-- | combine all LaTeX texts into a text file 
combineLaTeXText :: MainCoroutine ()
combineLaTeXText = do
    -- liftIO $ putStrLn "start combine latex file" 
    hdl <- rHoodle2Hoodle . getHoodle <$> get  
    let mlatex_components = do 
          (pgnum,pg) <- (zip ([1..] :: [Int]) . view pages) hdl  
          l <- view layers pg
          i <- view items l
          case i of 
            ItemSVG svg ->  
              case svg_command svg of
                Just "latex" -> do 
                  let (_,y) = svg_pos svg  
                  return ((pgnum,y,) <$> svg_text svg) 
                _ -> []
            _ -> []
    let cfunc :: (Ord a,Ord b,Ord c) => (a,b,c) -> (a,b,c) -> Ordering 
        cfunc x y | view _1 x > view _1 y = GT
                  | view _1 x < view _1 y = LT
                  | otherwise = if | view _2 x > view _2 y -> GT
                                   | view _2 x < view _2 y -> LT
                                   | otherwise -> EQ
    let latex_components = catMaybes  mlatex_components
        sorted = sortBy cfunc latex_components 
        resulttxt = (B.intercalate "%%%%%%%%%%%%\n\n%%%%%%%%%%\n" . map (view _3)) sorted
    mfilename <- fileChooser Gtk.FileChooserActionSave Nothing
    forM_ mfilename (\filename -> liftIO (B.writeFile filename resulttxt) >> return ())



insertItemAt :: Maybe (PageNum,PageCoordinate) 
                -> RItem 
                -> MainCoroutine () 
insertItemAt mpcoord ritm = do 
    xst <- get   
    geometry <- liftIO (getGeometry4CurrCvs xst) 
    let hdl = getHoodle xst 
        (pgnum,mpos) = case mpcoord of 
          Just (PageNum n,pos) -> (n,Just pos)
          Nothing -> (view (currentCanvasInfo . unboxLens currentPageNum) xst,Nothing)
        (ulx,uly) = (bbox_upperleft.getBBox) ritm
        nitms = 
          case mpos of 
            Nothing -> adjustItemPosition4Paste geometry (PageNum pgnum) [ritm] 
            Just (PageCoord (nx,ny)) -> 
                   map (changeItemBy (\(x,y)->(x+nx-ulx,y+ny-uly))) [ritm]
          
    let pg = getPageFromGHoodleMap pgnum hdl
        lyr = getCurrentLayer pg 
        oitms = view gitems lyr  
        ntpg = makePageSelectMode pg (oitms :- (Hitted nitms) :- Empty)  
    modeChange ToSelectMode 
    nxst <- get 
    let cache = view renderCache nxst
    thdl <- case view hoodleModeState nxst of
      SelectState thdl' -> return thdl'
      _ -> (lift . EitherT . return . Left . Other) "insertItemAt"
    nthdl <- liftIO $ updateTempHoodleSelectIO cache thdl ntpg pgnum 
    put ( ( set hoodleModeState (SelectState nthdl) 
          . set isOneTimeSelectMode YesAfterSelect) nxst)
    invalidateAll  

embedTextSource :: MainCoroutine ()
embedTextSource = do 
    mfilename <- fileChooser Gtk.FileChooserActionOpen Nothing
    forM_ mfilename $ \filename -> do 
      txt <- liftIO $ TIO.readFile filename
      xst <- get
      let nhdlmodst = case xst ^. hoodleModeState of
            ViewAppendState hdl -> (ViewAppendState . (gembeddedtext .~ Just txt) $ hdl)
            SelectState thdl    -> (SelectState     . (gselEmbeddedText .~ Just txt) $ thdl)
          nxst = (hoodleModeState .~ nhdlmodst) xst
      put nxst
      commit_ 


-- | insert text 
textInputFromSource :: (Double,Double) -> MainCoroutine ()
textInputFromSource (x0,y0) = do
    runMaybeT $ do 
      txtsrc <- MaybeT $ (^. gembeddedtext) . getHoodle <$> get
      lift $ modify (tempQueue %~ enqueue linePosDialog)  
      (l1,l2) <- MaybeT linePosLoop
      let txt = getLinesFromText (l1,l2) txtsrc
      lift $ deleteSelection
      liftIO (makePangoTextSVG (x0,y0) txt)
        >>= lift . svgInsert ("embedtxt:simple:L" <> T.pack (show l1) <> "," <> T.pack (show l2),"pango") 
    return ()

-- | common dialog with line position 
linePosDialog :: Either (ActionOrder AllEvent) AllEvent
linePosDialog = mkIOaction $ \evhandler -> do
    dialog <- Gtk.dialogNew
    vbox <- Gtk.dialogGetUpper dialog

    hbox <- Gtk.hBoxNew False 0
    Gtk.boxPackStart vbox hbox Gtk.PackNatural 0

    line1buf <- Gtk.entryBufferNew Nothing
    line1 <- Gtk.entryNewWithBuffer line1buf
    Gtk.boxPackStart hbox line1 Gtk.PackNatural 2

    line2buf <- Gtk.entryBufferNew Nothing
    line2 <- Gtk.entryNewWithBuffer line2buf
    Gtk.boxPackStart hbox line2 Gtk.PackNatural 2
    -- 
    _btnOk <- Gtk.dialogAddButton dialog "Ok" Gtk.ResponseOk
    _btnCancel <- Gtk.dialogAddButton dialog "Cancel" Gtk.ResponseCancel
    Gtk.widgetShowAll dialog
    res <- Gtk.dialogRun dialog
    Gtk.widgetDestroy dialog
    case res of 
      Gtk.ResponseOk -> do
        line1str <- B.pack <$> Gtk.get line1buf Gtk.entryBufferText
        line2str <- B.pack <$> Gtk.get line2buf Gtk.entryBufferText
        let el1l2 = (,) <$> parseOnly decimal line1str 
                        <*> parseOnly decimal line2str
        return . UsrEv . LinePosition 
         . either (const Nothing) (\(l1,l2)->if l1 <= l2 then Just (l1,l2) else Nothing) $ el1l2
      Gtk.ResponseCancel -> return (UsrEv (LinePosition Nothing))
      _ -> return (UsrEv (LinePosition Nothing))

-- | main event loop for line position dialog
linePosLoop :: MainCoroutine (Maybe (Int,Int))
linePosLoop = do 
    r <- nextevent
    case r of 
      UpdateCanvas cid -> invalidateInBBox Nothing Efficient cid >> linePosLoop
      LinePosition x -> return x
      _ -> linePosLoop


-- | insert text 
laTeXInputFromSource :: (Double,Double) -> MainCoroutine ()
laTeXInputFromSource (x0,y0) = do
    liftIO $ putStrLn "HELLO"
    runMaybeT $ do 
      txtsrc <- MaybeT $ (^. gembeddedtext) . getHoodle <$> get
      lift $ modify (tempQueue %~ enqueue keywordDialog)  
      keyword <- MaybeT keywordLoop
      subpart <- (MaybeT . return . M.lookup keyword . getKeywordMap) txtsrc
      liftIO (makeLaTeXSVG (x0,y0) subpart) >>= \case
        Right r  -> lift $ do 
                      deleteSelection 
                      svgInsert ("embedlatex:keyword:"<>keyword,"latex") r
        Left err -> lift $ do
                      okMessageBox err
                      return ()
      -- liftIO $ putStrLn " keyword = "
      -- liftIO $ print keyword
      -- liftIO $ putStrLn " subpart = " 
      -- liftIO $ print subpart 
      -- liftIO $ print keyword
      {- 
      let txt = getLinesFromText (l1,l2) txtsrc
      lift $ deleteSelection
      liftIO (makePangoTextSVG (x0,y0) txt)
        >>= lift . svgInsert ("embedtxt:simple:L" <> T.pack (show l1) <> "," <> T.pack (show l2),"pango")  -}
    return ()

-- | common dialog with line position 
keywordDialog :: Either (ActionOrder AllEvent) AllEvent
keywordDialog = mkIOaction $ \evhandler -> do
    dialog <- Gtk.dialogNew
    vbox <- Gtk.dialogGetUpper dialog
    hbox <- Gtk.hBoxNew False 0
    Gtk.boxPackStart vbox hbox Gtk.PackNatural 0
    keybuf <- Gtk.entryBufferNew Nothing
    key <- Gtk.entryNewWithBuffer keybuf
    Gtk.boxPackStart hbox key Gtk.PackNatural 2
    -- 
    _btnOk <- Gtk.dialogAddButton dialog "Ok" Gtk.ResponseOk
    _btnCancel <- Gtk.dialogAddButton dialog "Cancel" Gtk.ResponseCancel
    Gtk.widgetShowAll dialog
    res <- Gtk.dialogRun dialog
    Gtk.widgetDestroy dialog
    case res of 
      Gtk.ResponseOk -> do
        keystr <- T.pack <$> Gtk.get keybuf Gtk.entryBufferText
        (return . UsrEv . Keyword . Just) keystr
      Gtk.ResponseCancel -> return (UsrEv (Keyword Nothing))
      _ -> return (UsrEv (Keyword Nothing))

-- | main event loop for line position dialog
keywordLoop :: MainCoroutine (Maybe T.Text)
keywordLoop = do 
    r <- nextevent
    case r of 
      UpdateCanvas cid -> invalidateInBBox Nothing Efficient cid >> keywordLoop
      Keyword x -> return x
      _ -> keywordLoop
