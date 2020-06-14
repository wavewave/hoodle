{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Hoodle.Coroutine.TextInput where

import           Control.Applicative
import qualified Control.Exception
import           Control.Lens (_2,_3,view,(%~),(^.),(.~))
import           Control.Monad.State hiding (mapM_, forM_)
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Maybe
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B 
import           Data.Foldable (mapM_, forM_)
import           Data.List (sortBy)
import qualified Data.HashMap.Strict as M
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import           Data.UUID.V4 (nextRandom)
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Cairo.SVG as RSVG
import qualified Graphics.UI.Gtk as Gtk
import           System.Directory 
import           System.Exit (ExitCode(..))
import           System.FilePath 
import           System.Process (readProcessWithExitCode)
#ifdef HUB
import           Control.Concurrent (killThread)
#endif
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
import           Hoodle.Coroutine.LaTeX
import           Hoodle.Coroutine.Mode
import           Hoodle.Coroutine.Page
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
#ifdef HUB
import           Hoodle.Coroutine.Network
#endif
-- 
import Prelude hiding (readFile,mapM_)


-- | common dialog with multiline edit input box 
multiLineDialog :: T.Text -> (AllEvent -> IO ()) -> IO AllEvent
multiLineDialog str evhandler = do
    dialog <- Gtk.dialogNew
    upper <- fmap Gtk.castToContainer (Gtk.dialogGetContentArea dialog)
    vbox <- Gtk.vBoxNew False 0
    Gtk.containerAdd upper vbox
    textbuf <- Gtk.textBufferNew Nothing
    Gtk.textBufferSetByteString textbuf (TE.encodeUtf8 str)
    textbuf `Gtk.on` Gtk.bufferChanged $ do 
        (s,e) <- (,) <$> Gtk.textBufferGetStartIter textbuf <*> Gtk.textBufferGetEndIter textbuf
        contents <- Gtk.textBufferGetByteString textbuf s e False
        (evhandler . UsrEv . MultiLine . MultiLineChanged) (TE.decodeUtf8 contents)
    textarea <- Gtk.textViewNewWithBuffer textbuf
    vscrbar <- Gtk.vScrollbarNew =<< Gtk.textViewGetVadjustment textarea
    hscrbar <- Gtk.hScrollbarNew =<< Gtk.textViewGetHadjustment textarea 
    Gtk.widgetSetSizeRequest textarea 500 600
    fdesc <- Gtk.fontDescriptionNew
    Gtk.fontDescriptionSetFamily fdesc ("Mono" :: String)
    Gtk.widgetModifyFont textarea (Just fdesc)
    -- 
    table <- Gtk.tableNew 2 2 False
    Gtk.tableAttachDefaults table textarea 0 1 0 1
    Gtk.tableAttachDefaults table vscrbar 1 2 0 1
    Gtk.tableAttachDefaults table hscrbar 0 1 1 2 
    Gtk.boxPackStart vbox table Gtk.PackNatural 0
    -- 
    _btnOk <- Gtk.dialogAddButton dialog ("Ok" :: String) Gtk.ResponseOk
    _btnCancel <- Gtk.dialogAddButton dialog ("Cancel" :: String) Gtk.ResponseCancel
#ifdef HUB
    _btnNetwork <- Gtk.dialogAddButton dialog ("Network" :: String) (Gtk.ResponseUser 1)
#endif
    Gtk.widgetShowAll dialog
    res <- Gtk.dialogRun dialog
    Gtk.widgetDestroy dialog
    case res of 
      Gtk.ResponseOk -> return (UsrEv (OkCancel True))
      Gtk.ResponseCancel -> return (UsrEv (OkCancel False))
#ifdef HUB
      Gtk.ResponseUser 1 -> return (UsrEv (NetworkProcess NetworkDialog))
#endif
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
#ifdef HUB
      NetworkProcess NetworkDialog -> networkTextInput txt
#endif
      MultiLine (MultiLineChanged txt') -> multiLineLoop txt'
      _ -> multiLineLoop txt

-- | insert text 
textInput :: Maybe (Double,Double) -> T.Text -> MainCoroutine ()
textInput mpos str = do 
    case mpos of 
      Just (x0,y0) -> do 
        doIOaction (multiLineDialog str)
        multiLineLoop str >>= 
          mapM_ (\result -> deleteSelection
                            >> liftIO (makePangoTextSVG (x0,y0) result) 
                            >>= svgInsert (result,"pango"))
      Nothing -> msgShout "textInput: not implemented"
  
-- | insert latex
laTeXInput :: Maybe (Double,Double) -> T.Text -> MainCoroutine ()
laTeXInput mpos str = do 
    case mpos of 
      Just (x0,y0) -> do 
        doIOaction (multiLineDialog str)
        multiLineLoop str >>= 
          mapM_ (\result -> liftIO (makeLaTeXSVG (x0,y0) Nothing result) >>= \case 
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
#ifdef HUB
laTeXInputNetwork :: Maybe (Double,Double) -> T.Text -> MainCoroutine ()
laTeXInputNetwork mpos str =  
    case mpos of 
      Just (x0,y0) -> do 
        networkTextInput str >>=
          mapM_ (\result -> liftIO (makeLaTeXSVG (x0,y0) Nothing result) 
                            >>= \case Right r -> deleteSelection >> svgInsert (result,"latex") r
                                      Left err -> okMessageBox err >> laTeXInput mpos result
                )
      Nothing -> do 
        modeChange ToViewAppendMode 
        autoPosText >>=
          maybe (laTeXInputNetwork (Just (100,100)) str) 
                (\y'->laTeXInputNetwork (Just (100,y')) str) 
#endif

dbusNetworkInput :: T.Text -> MainCoroutine ()
dbusNetworkInput txt = do 
    modeChange ToViewAppendMode     
    mpos <- autoPosText 
    let pos = maybe (100,100) (100,) mpos 
    rsvg <- liftIO (makeLaTeXSVG pos Nothing txt) 
    case rsvg of 
      Right r -> deleteSelection >> svgInsert (txt,"latex") r
      Left err -> okMessageBox err >> laTeXInput (Just pos) txt


check :: String -> IO (ExitCode,String) -> EitherT String IO ()
check msg act = do
    er <- liftIO $ Control.Exception.try act
    case er of 
      Left (Control.Exception.SomeException excp) -> left (show excp)
      Right (ecode,str) -> case ecode of ExitSuccess -> right () ; _ -> left (msg ++ ":" ++ str)


makeLaTeXSVG :: (Double,Double) -> Maybe Dimension -> T.Text 
             -> IO (Either String (B.ByteString,BBox))
makeLaTeXSVG (x0,y0) mdim txt = do
    cdir <- getCurrentDirectory
    tdir <- getTemporaryDirectory
    tfilename <- show <$> nextRandom
    setCurrentDirectory tdir
    B.writeFile (tfilename <.> "tex") (TE.encodeUtf8 txt)
    r <- runEitherT $ do 
      check "error during xelatex" $ do 
        (ecode,ostr,estr) <- readProcessWithExitCode "xelatex" ["-shell-escape", tfilename <.> "tex"] ""
        return (ecode,ostr++estr)
      check "error during pdfcrop" $ do 
        (ecode,ostr,estr) <- readProcessWithExitCode "pdfcrop" [tfilename <.> "pdf",tfilename ++ "_crop" <.> "pdf"] ""       
        return (ecode,ostr++estr)
      check "error during pdf2svg" $ do
        (ecode,ostr,estr) <- readProcessWithExitCode "pdf2svg" [tfilename ++ "_crop" <.> "pdf",tfilename <.> "svg"] ""
        return (ecode,ostr++estr)
      bstr <- liftIO $ B.readFile (tfilename <.> "svg")
      rsvg <- liftIO $ RSVG.svgNewFromString (B.unpack bstr) 
      let ow, oh :: Int
          (ow,oh) = RSVG.svgGetSize rsvg
          ow', oh' :: Double
          (ow',oh') = (fromIntegral ow, fromIntegral oh)
          w, h :: Double
          (w,h) = maybe (ow',oh') (\(Dim w' _)->(w',oh'*w'/ow')) mdim 
      return (bstr,BBox (x0,y0) (x0+w,y0+h)) 
    setCurrentDirectory cdir
    return r
    

-- |
svgInsert :: (T.Text,String) -> (B.ByteString,BBox) -> MainCoroutine () 
svgInsert (txt,cmd) (svgbstr,BBox (x0,y0) (x1,y1)) = do 
    xst <- get 
    let uhdl = view (unitHoodles.currentUnit) xst
        cid = getCurrentCanvasId uhdl
        pgnum = view (unboxLens currentPageNum) . view currentCanvasInfo $ uhdl
        hdl = getHoodle uhdl
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
    modeChange ToSelectMode 
    updateUhdl $ \nuhdl -> do
      thdl <- case view hoodleModeState nuhdl of
                SelectState thdl' -> return thdl'
                _ -> (lift . EitherT . return . Left . Other) "svgInsert"
      nthdl <- updateTempHoodleSelectM cid thdl ntpg pgnum 
      return $ (hoodleModeState .~ SelectState nthdl) nuhdl
    commit_
    canvasZoomUpdateAll
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
    xst <- get 
    let uhdl = view (unitHoodles.currentUnit) xst 
    let pgnum = view (currentCanvasInfo . unboxLens currentPageNum) uhdl
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
    hdl <- rHoodle2Hoodle . getHoodle . view (unitHoodles.currentUnit) <$> get  
    let sorted = getLaTeXComponentsFromHdl hdl
   
        resulttxt = (T.intercalate "%%%%%%%%%%%%\n\n%%%%%%%%%%\n" . map (view (_2._3))) sorted
    mfilename <- fileChooser Gtk.FileChooserActionSave Nothing
    forM_ mfilename (\filename -> liftIO (TIO.writeFile filename resulttxt) >> return ())


insertItemAt :: Maybe (PageNum,PageCoordinate) 
                -> RItem 
                -> MainCoroutine () 
insertItemAt mpcoord ritm = do 
    xst <- get   
    let uhdl = view (unitHoodles.currentUnit) xst
    geometry <- liftIO (getGeometry4CurrCvs uhdl) 
    let cid = getCurrentCanvasId uhdl
        hdl = getHoodle uhdl
        (pgnum,mpos) = case mpcoord of 
          Just (PageNum n,pos) -> (n,Just pos)
          Nothing -> (view (currentCanvasInfo . unboxLens currentPageNum) uhdl,Nothing)
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
    updateUhdl $ \uhdl' -> do 
      thdl <- case view hoodleModeState uhdl' of
        SelectState thdl' -> return thdl'
        _ -> (lift . EitherT . return . Left . Other) "insertItemAt"
      nthdl <- updateTempHoodleSelectM cid thdl ntpg pgnum 
      return . (hoodleModeState .~ SelectState nthdl)
             . (isOneTimeSelectMode .~ YesAfterSelect) $ uhdl' 
    commit_
    canvasZoomUpdateAll
    invalidateAll  

embedTextSource :: MainCoroutine ()
embedTextSource = do 
    mfilename <- fileChooser Gtk.FileChooserActionOpen Nothing
    forM_ mfilename $ \filename -> do 
      txt <- liftIO $ TIO.readFile filename
      pureUpdateUhdl $ \uhdl ->
        let nhdlmodst = case uhdl ^. hoodleModeState of
              ViewAppendState hdl -> (ViewAppendState . (gembeddedtext .~ Just txt) $ hdl)
              SelectState thdl    -> (SelectState     . (gselEmbeddedText .~ Just txt) $ thdl)
        in (hoodleModeState .~ nhdlmodst) uhdl
      commit_ 

-- |
editEmbeddedTextSource :: MainCoroutine ()
editEmbeddedTextSource = do 
    hdl <- getHoodle . view (unitHoodles.currentUnit) <$> get
    let mtxt = hdl ^. gembeddedtext 
    forM_ mtxt $ \txt -> do 
      doIOaction (multiLineDialog txt)
      multiLineLoop txt >>= \case 
        Nothing -> return ()
        Just ntxt -> do 
          pureUpdateUhdl $ \uhdl ->
            let nhdlmodst = case uhdl ^. hoodleModeState of
                  ViewAppendState hdl' -> (ViewAppendState . (gembeddedtext .~ Just ntxt) $ hdl')
                  SelectState thdl    -> (SelectState     . (gselEmbeddedText .~ Just ntxt) $ thdl)
            in (hoodleModeState .~ nhdlmodst) uhdl
          commit_


-- |
#ifdef HUB
editNetEmbeddedTextSource :: MainCoroutine ()
editNetEmbeddedTextSource = do 
    hdl <- getHoodle . view (unitHoodles.currentUnit) <$> get
    let mtxt = hdl ^. gembeddedtext 
    forM_ mtxt $ \txt -> do 
      networkTextInput txt >>= \case 
        Nothing -> return ()
        Just ntxt -> networkReceived ntxt
#endif

#ifdef HUB
networkReceived :: T.Text -> MainCoroutine ()
networkReceived txt = do
    pureUpdateUhdl $ \uhdl -> 
      let nhdlmodst = case uhdl ^. hoodleModeState of
            ViewAppendState hdl -> (ViewAppendState . (gembeddedtext .~ Just txt) $ hdl)
            SelectState thdl    -> (SelectState     . (gselEmbeddedText .~ Just txt) $ thdl)
      in (hoodleModeState .~ nhdlmodst) uhdl
    commit_
#endif

-- | insert text 
textInputFromSource :: (Double,Double) -> MainCoroutine ()
textInputFromSource (x0,y0) = do
    runMaybeT $ do 
      txtsrc <- MaybeT $ (^. gembeddedtext) . getHoodle . view (unitHoodles.currentUnit) <$> get
      lift $ modify (tempQueue %~ enqueue linePosDialog)  
      (l1,l2) <- MaybeT linePosLoop
      let txt = getLinesFromText (l1,l2) txtsrc
      lift $ deleteSelection
      liftIO (makePangoTextSVG (x0,y0) txt)
        >>= lift . svgInsert ("embedtxt:simple:L" <> T.pack (show l1) <> "," <> T.pack (show l2),"pango") 
    return ()

-- | common dialog with line position 
linePosDialog :: Either (ActionOrder AllEvent) AllEvent
linePosDialog = mkIOaction $ \_evhandler -> do
    dialog <- Gtk.dialogNew
    upper <- fmap Gtk.castToContainer (Gtk.dialogGetContentArea dialog)
    vbox <- Gtk.vBoxNew False 0 
    Gtk.containerAdd upper vbox
    hbox <- Gtk.hBoxNew False 0
    Gtk.boxPackStart vbox hbox Gtk.PackNatural 0

    line1buf <- Gtk.entryBufferNew (Nothing :: Maybe String)
    line1 <- Gtk.entryNewWithBuffer line1buf
    Gtk.boxPackStart hbox line1 Gtk.PackNatural 2

    line2buf <- Gtk.entryBufferNew (Nothing :: Maybe String)
    line2 <- Gtk.entryNewWithBuffer line2buf
    Gtk.boxPackStart hbox line2 Gtk.PackNatural 2
    -- 
    _btnOk <- Gtk.dialogAddButton dialog ("Ok" :: String) Gtk.ResponseOk
    _btnCancel <- Gtk.dialogAddButton dialog ("Cancel" :: String) Gtk.ResponseCancel
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
laTeXInputKeyword :: (Double,Double) -> Maybe Dimension -> T.Text 
                  -> MaybeT MainCoroutine ()
laTeXInputKeyword (x0,y0) mdim keyword = do
    txtsrc <- MaybeT $ (^. gembeddedtext) . getHoodle . view (unitHoodles.currentUnit) <$> get
    subpart <- (MaybeT . return . M.lookup keyword . getKeywordMap) txtsrc
    let subpart' = laTeXHeader <> "\n"  <> subpart <> laTeXFooter
    liftIO (makeLaTeXSVG (x0,y0) mdim subpart') >>= \case
      Right r  -> lift $ do 
                    deleteSelection 
                    svgInsert ("embedlatex:keyword:"<>keyword,"latex") r
      Left err -> lift $ do
                    longTextMessageBox err
                    -- okMessageBox err
                    return ()

-- | 
laTeXInputFromSource :: (Double,Double) -> MainCoroutine ()
laTeXInputFromSource (x0,y0) = do
    runMaybeT $ do 
      txtsrc <- MaybeT $ (^. gembeddedtext) . getHoodle . view (unitHoodles.currentUnit) <$> get
      let keylst = (map fst . M.toList . getKeywordMap) txtsrc
      when ((not.null) keylst) $ do 
        keyword <- MaybeT (keywordDialog keylst)
        laTeXInputKeyword (x0,y0) Nothing keyword
    return ()
    
-- | 
#ifdef HUB
toggleNetworkEditSource :: MainCoroutine ()
toggleNetworkEditSource = do 
    xst <- get
    let uhdl = view (unitHoodles.currentUnit) xst
        hdl = getHoodle uhdl
        mtxt = hdl ^. gembeddedtext
    b <- updateFlagFromToggleUI "TOGGLENETSRCA" (settings.doesUseXInput)
    forM_ (xst ^. statusBar) $ \stbar -> 
      if b 
        then do
          (ip,tid,_done) <- networkTextInputBody (maybe " " id mtxt)
          doIOaction_ $ do
            let msg = ("networkedit " ++ ip ++ " 4040")
            ctxt <- Gtk.statusbarGetContextId stbar ("networkedit" :: String)
            Gtk.statusbarPush stbar ctxt msg
          (put . ((settings.networkEditSourceInfo) .~ (Just tid))) xst
        else do
          case xst ^. (settings.networkEditSourceInfo) of
            Nothing -> return ()
            Just tid -> do
              doIOaction_ $ do
                killThread tid
                ctxt <- Gtk.statusbarGetContextId stbar ("networkedit" :: String)
                Gtk.statusbarPush stbar ctxt ("Now no networkedit" :: String)
              (put . ((settings.networkEditSourceInfo) .~ Nothing)) xst
#endif
      

