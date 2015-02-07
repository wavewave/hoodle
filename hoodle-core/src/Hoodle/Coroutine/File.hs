{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.File 
-- Copyright   : (c) 2011-2015 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.File where

-- from other packages
import           Control.Applicative ((<$>),(<*>))
import           Control.Concurrent
import           Control.Lens (at,view,set,over,(.~))
import           Control.Monad.State hiding (mapM,mapM_,forM_)
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.Attoparsec.ByteString.Char8 (parseOnly)
import           Data.ByteString.Char8 as B (pack,unpack,readFile)
import qualified Data.ByteString.Lazy as L
import           Data.Digest.Pure.MD5 (md5)
import           Data.Foldable (mapM_,forM_)
import qualified Data.List as List 
import           Data.Maybe
import qualified Data.Text.Encoding as TE
import           Data.Time.Clock
import           Filesystem.Path.CurrentOS (decodeString, encodeString)
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.UI.Gtk as Gtk 
import           System.Directory
import           System.FilePath
import qualified System.FSNotify as FS
import           System.IO (hClose, hFileSize, openFile, IOMode(..))
import           System.Process 
-- from hoodle-platform
import           Control.Monad.Trans.Crtn
-- import           Data.Hoodle.BBox
import           Data.Hoodle.Generic
import           Data.Hoodle.Simple
import           Data.Hoodle.Select
import           Graphics.Hoodle.Render (Xform4Page(..),cnstrctRHoodle)
import           Graphics.Hoodle.Render.Generic
import           Graphics.Hoodle.Render.Item
import           Graphics.Hoodle.Render.Type
import           Graphics.Hoodle.Render.Type.HitTest 
import           Hoodle.Publish.PDF (renderHoodleToPDF)
import           Text.Hoodle.Builder 
import           Text.Hoodle.Migrate.FromXournal
import qualified Text.Hoodlet.Parse.Attoparsec as Hoodlet
import qualified Text.Xournal.Parse.Conduit as XP
-- from this package 
import           Hoodle.Accessor
import           Hoodle.Coroutine.Dialog
import           Hoodle.Coroutine.Draw
import           Hoodle.Coroutine.Commit
import           Hoodle.Coroutine.Layer
import           Hoodle.Coroutine.Minibuffer
import           Hoodle.Coroutine.Mode 
import           Hoodle.Coroutine.Page
import           Hoodle.Coroutine.Scroll
import           Hoodle.Coroutine.Select.Clipboard
import           Hoodle.Coroutine.TextInput
-- import           Hoodle.Coroutine.Window
import           Hoodle.GUI.Reflect
import           Hoodle.ModelAction.File
import           Hoodle.ModelAction.Layer 
import           Hoodle.ModelAction.Page
import           Hoodle.ModelAction.Select
import           Hoodle.ModelAction.Window
import qualified Hoodle.Script.Coroutine as S
import           Hoodle.Script.Hook
import           Hoodle.Type.Canvas
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Enum
import           Hoodle.Type.Event hiding (TypSVG)
import           Hoodle.Type.HoodleState
import           Hoodle.Type.PageArrangement
import           Hoodle.Util
#ifdef HUB
import           Hoodle.Coroutine.Hub
import           Hoodle.Coroutine.Hub.Common
#endif
--
import Prelude hiding (readFile,concat,mapM,mapM_)

-- | 
askIfSave :: MainCoroutine () -> MainCoroutine () 
askIfSave action = do 
    uhdl <- view (unitHoodles.currentUnit) <$> get 
    if not (view isSaved uhdl)
      then 
        okCancelMessageBox "Current canvas is not saved yet. Will you proceed without save?" >>= flip when action
      else action 

-- | 
askIfOverwrite :: FilePath -> MainCoroutine () -> MainCoroutine () 
askIfOverwrite fp action = do 
    b <- liftIO $ doesFileExist fp 
    if b 
      then do 
        r <- okCancelMessageBox ("Overwrite " ++ fp ++ "???") 
        if r then action else return () 
      else action 


-- | get file content from xournal file and update xournal state 
getFileContent :: Maybe FilePath -> MainCoroutine ()
getFileContent (Just fname) = do 
    xstate <- get
    let ext = takeExtension fname
    case ext of 
      ".hdl" -> do 
        bstr <- liftIO $ B.readFile fname
        r <- liftIO $ checkVersionAndMigrate bstr 
        case r of 
          Left err -> liftIO $ putStrLn err
          Right h -> do 
            constructNewHoodleStateFromHoodle h
            ctime <- liftIO $ getCurrentTime
#ifdef HUB
            msqlfile <- view (settings.sqliteFileName) <$> get
#endif    

#ifdef HUB
            mmd5 <- case msqlfile of
              Nothing -> return Nothing
              Just sqlfile -> (liftIO . getLastSyncMD5 sqlfile . TE.decodeUtf8 . view hoodleID) h
#else 
            let mmd5 = Nothing 
#endif    

            pureUpdateUhdl ( (hoodleFileControl.hoodleFileName .~ Just fname)
                           . (hoodleFileControl.lastSavedTime  .~ Just ctime) 
                           . (hoodleFileControl.lastSyncMD5 .~ mmd5) 
                           )
            commit_
      ".xoj" -> do 
          liftIO (XP.parseXojFile fname) >>= \x -> case x of  
            Left str -> msgShout $ "file reading error : " ++ str 
            Right xojcontent -> do 
              hdlcontent <- liftIO $ mkHoodleFromXournal xojcontent 
              constructNewHoodleStateFromHoodle hdlcontent
              ctime <- liftIO $ getCurrentTime 
              pureUpdateUhdl ( (hoodleFileControl.hoodleFileName .~ Just fname)
                             . (hoodleFileControl.lastSavedTime  .~ Just ctime) )
              commit_
      ".pdf" -> do 
        let doesembed = view (settings.doesEmbedPDF) xstate
        mhdl <- liftIO $ makeNewHoodleWithPDF doesembed fname 
        case mhdl of 
          Nothing -> getFileContent Nothing
          Just hdl -> do 
            constructNewHoodleStateFromHoodle hdl
            pureUpdateUhdl (hoodleFileControl.hoodleFileName .~ Nothing)
            commit_
      _ -> getFileContent Nothing    
    xstate' <- get
    doIOaction_ $ Gtk.postGUIAsync (setTitleFromFileName xstate')
getFileContent Nothing = do
    constructNewHoodleStateFromHoodle =<< liftIO defaultHoodle 
    pureUpdateUhdl (hoodleFileControl.hoodleFileName .~ Nothing) 
    commit_ 

-- |
constructNewHoodleStateFromHoodle :: Hoodle -> MainCoroutine ()  
constructNewHoodleStateFromHoodle hdl' = do 
    callRenderer $ cnstrctRHoodle hdl' >>= return . GotRHoodle
    RenderEv (GotRHoodle rhdl) <- waitSomeEvent (\case RenderEv (GotRHoodle _) -> True; _ -> False)
    pureUpdateUhdl (hoodleModeState .~ ViewAppendState rhdl)

-- | deprecated
fileNew :: MainCoroutine () 
fileNew = do 
    getFileContent Nothing
    updateUhdl $ \uhdl -> do
      ncvsinfo <- liftIO $ setPage uhdl 0 (getCurrentCanvasId uhdl)
      return $ (currentCanvasInfo .~ ncvsinfo) uhdl
    xst <- get
    liftIO (setTitleFromFileName xst)
    commit_
    invalidateAll 

-- | 
fileSave :: MainCoroutine ()
fileSave = do 
    uhdl <- view (unitHoodles.currentUnit) <$> get
    case view (hoodleFileControl.hoodleFileName) uhdl of
      Nothing -> fileSaveAs 
      Just filename -> do     
        -- this is rather temporary not to make mistake 
        if takeExtension filename == ".hdl" 
          then do 
             updateUhdl $ liftIO . saveHoodle . const uhdl 
             (S.afterSaveHook filename . rHoodle2Hoodle . getHoodle) uhdl
#ifdef HUB
             hubUpload
#endif
          else fileExtensionInvalid (".hdl","save") >> fileSaveAs 

-- | interleaving a monadic action between each pair of subsequent actions
sequence1_ :: (Monad m) => m () -> [m ()] -> m () 
sequence1_ _ []  = return () 
sequence1_ _ [a] = a 
sequence1_ i (a:as) = a >> i >> sequence1_ i as 

-- | 
fileExport :: MainCoroutine ()
fileExport = fileChooser Gtk.FileChooserActionSave Nothing >>= maybe (return ()) action 
  where 
    action filename = do
      -- this is rather temporary not to make mistake 
      if takeExtension filename /= ".pdf" 
        then fileExtensionInvalid (".pdf","export") >> fileExport 
        else do      
          hdl <- rHoodle2Hoodle . getHoodle . view (unitHoodles.currentUnit) <$> get
          liftIO (renderHoodleToPDF hdl filename) 

-- | 
fileStartSync :: MainCoroutine ()
fileStartSync = do 
  uhdl <- view (unitHoodles.currentUnit) <$> get 
  let mf = (,) <$> view (hoodleFileControl.hoodleFileName) uhdl <*> view (hoodleFileControl.lastSavedTime) uhdl 
  maybe (return ()) (\(filename,lasttime) -> action filename lasttime) mf  
  where  
    action filename _lasttime  = doIOaction $ \evhandler -> do 
            forkIO $ do 
              FS.withManager $ \wm -> do 
                origfile <- canonicalizePath filename 
                let (filedir,_) = splitFileName origfile
                print filedir 
                _ <- FS.watchDir wm (decodeString filedir) (const True) $ \ev -> do
                  let mchangedfile = case ev of 
                        FS.Added fp _ -> Just (encodeString fp)
                        FS.Modified fp _ -> Just (encodeString fp)
                        FS.Removed _fp _ -> Nothing 
                  print mchangedfile 
                  case mchangedfile of 
                    Nothing -> return ()
                    Just changedfile -> do                       
                      let changedfilename = takeFileName changedfile 
                          changedfile' = (filedir </> changedfilename)
                      if changedfile' == origfile 
                        then do 
                          ctime <- getCurrentTime 
                          evhandler (UsrEv (Sync ctime))
                        else return () 

                let sec = 1000000
                forever (threadDelay (100 * sec))
            return (UsrEv ActionOrdered)

-- | need to be merged with ContextMenuEventSVG
exportCurrentPageAsSVG :: MainCoroutine ()
exportCurrentPageAsSVG = fileChooser Gtk.FileChooserActionSave Nothing >>= maybe (return ()) action 
  where 
    action filename = 
      -- this is rather temporary not to make mistake 
      if takeExtension filename /= ".svg" 
      then fileExtensionInvalid (".svg","export") >> exportCurrentPageAsSVG 
      else do
        cid <- getCurrentCanvasId . view (unitHoodles.currentUnit) <$> get
        cache <- renderCache
        cpg <- getCurrentPageCurr
        let Dim w h = view gdimension cpg 
        liftIO $ Cairo.withSVGSurface filename w h $ \s -> Cairo.renderWith s $ 
         cairoRenderOption (InBBoxOption Nothing) cache cid (InBBox cpg,Nothing :: Maybe Xform4Page) >> return ()

-- | 
fileLoad :: FilePath -> MainCoroutine () 
fileLoad filename = do
    getFileContent (Just filename)
    updateUhdl $ \uhdl -> do 
      ncvsinfo <- liftIO $ setPage uhdl 0 (getCurrentCanvasId uhdl)
      return . (currentCanvasInfo .~ ncvsinfo) . (isSaved .~ True) $ uhdl
    xst <- get 
    let ui = view gtkUIManager xst
    liftIO $ reflectUIToggle ui "SAVEA" False
    liftIO $ setTitleFromFileName xst
    clearUndoHistory 
    modeChange ToViewAppendMode 
    resetHoodleBuffers 
    invalidateAll 
    applyActionToAllCVS adjustScrollbarWithGeometryCvsId

-- | 
resetHoodleBuffers :: MainCoroutine () 
resetHoodleBuffers = do 
    updateUhdl $ \uhdl -> do 
      let hdlst = view hoodleModeState uhdl
          cid = getCurrentCanvasId uhdl
      callRenderer_ $ resetHoodleModeStateBuffers cid hdlst
      return . (hoodleModeState .~ hdlst) $ uhdl


-- | main coroutine for open a file 
fileOpen :: MainCoroutine ()
fileOpen = do 
    mfilename <- fileChooser Gtk.FileChooserActionOpen Nothing
    forM_ mfilename fileLoad 

-- | main coroutine for save as 
fileSaveAs :: MainCoroutine () 
fileSaveAs = do 
    hdl <- (rHoodle2Hoodle . getHoodle . view (unitHoodles.currentUnit)) <$> get
    maybe (defSaveAsAction hdl) (\f -> liftIO (f hdl)) =<< hookSaveAsAction
  where 
    hookSaveAsAction = (saveAsHook <=< view hookSet) <$> get
    msuggestedact = (fileNameSuggestionHook <=< view hookSet) <$> get
    defSaveAsAction hdl = do 
        (msuggested :: Maybe String) <- maybe (return Nothing) (liftM Just . liftIO) =<< msuggestedact 
        mr <- fileChooser Gtk.FileChooserActionSave msuggested 
        maybe (return ()) (action hdl) mr 
      where action hd filename = 
              if takeExtension filename /= ".hdl" 
              then fileExtensionInvalid (".hdl","save")
              else do 
                askIfOverwrite filename $ do
                  updateUhdl $ \uhdl -> do
                    let ntitle = B.pack . snd . splitFileName $ filename 
                        (hdlmodst',hdl') = case view hoodleModeState uhdl of
                           ViewAppendState hdlmap -> 
                             if view gtitle hdlmap == "untitled"
                               then ( ViewAppendState . set gtitle ntitle
                                      $ hdlmap
                                    , (set title ntitle hd))
                               else (ViewAppendState hdlmap,hd)
                           SelectState thdl -> 
                             if view gselTitle thdl == "untitled"
                               then ( SelectState $ set gselTitle ntitle thdl 
                                    , set title ntitle hd)  
                               else (SelectState thdl,hd)
                    liftIO . L.writeFile filename . builder $ hdl'
                    return . (hoodleFileControl.hoodleFileName .~ Just filename)
                           . (hoodleModeState .~ hdlmodst') 
                           . (isSaved .~ True)
                           $ uhdl
                xst <- get
                let ui = view gtkUIManager xst
                    hdl'' = (rHoodle2Hoodle . getHoodle . view (unitHoodles.currentUnit)) xst
                -- liftIO $ toggleSave ui False
                liftIO $ reflectUIToggle ui "SAVEA" False
                liftIO $ setTitleFromFileName xst
                S.afterSaveHook filename hdl''
#ifdef HUB
                hubUpload          
#endif

-- | main coroutine for open a file 
fileReload :: MainCoroutine ()
fileReload = do
    uhdl <- view (unitHoodles.currentUnit) <$> get
    case view (hoodleFileControl.hoodleFileName) uhdl of 
      Nothing -> return ()
      Just filename -> do
        if not (view isSaved uhdl) 
          then do
            b <- okCancelMessageBox "Discard changes and reload the file?" 
            case b of 
              True -> fileLoad filename 
              False -> return ()
          else fileLoad filename

-- | 
fileExtensionInvalid :: (String,String) -> MainCoroutine ()
fileExtensionInvalid (ext,a) = 
  okMessageBox $ "only " 
                 ++ ext 
                 ++ " extension is supported for " 
                 ++ a 
    
-- | 
fileAnnotatePDF :: MainCoroutine ()
fileAnnotatePDF = 
    fileChooser Gtk.FileChooserActionOpen Nothing >>= maybe (return ()) action 
  where 
    warning = do 
      okMessageBox "cannot load the pdf file. Check your hoodle compiled with poppler library" 
      invalidateAll 
    action filename = do  
      xstate <- get 
      let doesembed = view (settings.doesEmbedPDF) xstate
      mhdl <- liftIO $ makeNewHoodleWithPDF doesembed filename 
      flip (maybe warning) mhdl $ \hdl -> do 
        constructNewHoodleStateFromHoodle hdl
        pureUpdateUhdl (hoodleFileControl.hoodleFileName .~ Nothing)
        commit_        
        setTitleFromFileName_ 
        canvasZoomUpdateAll
        -- invalidateAll  
      

-- | set frame title according to file name
setTitleFromFileName_ :: MainCoroutine () 
setTitleFromFileName_ = get >>= liftIO . setTitleFromFileName



-- |
checkEmbedImageSize :: FilePath -> MainCoroutine (Maybe FilePath) 
checkEmbedImageSize filename = do 
  xst <- get 
  runMaybeT $ do 
    sizelimit <- (MaybeT . return) (warningEmbedImageSize =<< view hookSet xst)
    siz <- liftIO $ do  
      h <- openFile filename ReadMode 
      s <- hFileSize h 
      hClose h
      return s 
    guard (siz > sizelimit) 
    let suggestscale :: Double = sqrt (fromIntegral sizelimit / fromIntegral siz) 
    b <- lift . okCancelMessageBox $ "The size of " ++ filename ++ "=" ++ show siz ++ "\nis bigger than limit=" ++ show sizelimit ++ ".\nWill you reduce size?"
    guard b 
    let ext = let x' = takeExtension filename 
              in if (not.null) x' then tail x' else "" 
    tmpfile <- liftIO $ mkTmpFile ext 
    cmd <- (MaybeT . return) (shrinkCmd4EmbedImage =<< view hookSet xst)    
    liftIO $ cmd suggestscale filename tmpfile
    return tmpfile 

-- | 
fileLoadPNGorJPG :: MainCoroutine ()
fileLoadPNGorJPG = do 
    fileChooser Gtk.FileChooserActionOpen Nothing >>= maybe (return ()) embedImage


-- | 
fileLoadImageBackground :: MainCoroutine ()
fileLoadImageBackground = do 
    fileChooser Gtk.FileChooserActionOpen Nothing >>= maybe (return ()) action
  where 
    action filename = do 
      xst <- get
      let fDoesEmbedImg = view (settings.doesEmbedImage) xst
          uhdl = view (unitHoodles.currentUnit) xst
          hdl = getHoodle uhdl
          Dim pw ph = (view gdimension . fromJust . view (gpages.at 0)) hdl 

      itm <- if fDoesEmbedImg 
               then checkEmbedImageSize filename 
                    >>= maybe (liftIO $ makeNewItemImage True filename) 
                              (liftIO . makeNewItemImage True)
               else liftIO (makeNewItemImage False filename)
      let ItemImage img = itm
          Dim w h = img_dim img
          ratio = h/w 
          ndim = Dim pw (pw*ratio)
          img' = img { img_dim = ndim }
      changePage (const 0)
      newPage (Just ndim) PageBefore
      callRenderer $ cnstrctRItem (ItemImage img') >>= return . GotRItem 
      RenderEv (GotRItem nitm) <- waitSomeEvent (\case RenderEv (GotRItem _) -> True ; _ -> False)
      insertItemAt (Just (PageNum 0, PageCoord (0,0))) nitm
      modeChange ToViewAppendMode
      makeNewLayer
{-
            --
            callRenderer $ case mf of  
              Nothing -> liftIO (makeNewItemImage True filename) >>= cnstrctRItem >>= return . GotRItem 
              Just f -> liftIO (makeNewItemImage True f) >>= cnstrctRItem >>= return . GotRItem
            RenderEv (GotRItem r) <- waitSomeEvent (\case RenderEv (GotRItem _) -> True ; _ -> False )
            return r
          else do 
            callRenderer $ liftIO (makeNewItemImage False filename) >>= cnstrctRItem >>= return . GotRItem
            RenderEv (GotRItem r) <- waitSomeEvent (\case RenderEv (GotRItem _) -> True ; _ -> False )
            return r

    let cpn = view (currentCanvasInfo . unboxLens currentPageNum) uhdl
    my <- autoPosText 
    let mpos = (\y->(PageNum cpn,PageCoord (50,y)))<$>my  
    insertItemAt mpos nitm 
  -}  


embedImage :: FilePath -> MainCoroutine ()
embedImage filename = do  
    xst <- get
    let fDoesEmbedImg = view (settings.doesEmbedImage) xst
        uhdl = view (unitHoodles.currentUnit) xst
    nitm <- 
      if fDoesEmbedImg 
        then do  
          mf <- checkEmbedImageSize filename 
          --
          callRenderer $ case mf of  
              Nothing -> liftIO (makeNewItemImage True filename) >>= cnstrctRItem >>= return . GotRItem 
              Just f -> liftIO (makeNewItemImage True f) >>= cnstrctRItem >>= return . GotRItem
          RenderEv (GotRItem r) <- waitSomeEvent (\case RenderEv (GotRItem _) -> True ; _ -> False )
          return r
        else do 
          callRenderer $ liftIO (makeNewItemImage False filename) >>= cnstrctRItem >>= return . GotRItem
          RenderEv (GotRItem r) <- waitSomeEvent (\case RenderEv (GotRItem _) -> True ; _ -> False )
          return r

    let cpn = view (currentCanvasInfo . unboxLens currentPageNum) uhdl
    my <- autoPosText 
    let mpos = (\y->(PageNum cpn,PageCoord (50,y)))<$>my  
    insertItemAt mpos nitm 

                    
-- | 
fileLoadSVG :: MainCoroutine ()
fileLoadSVG = do 
    fileChooser Gtk.FileChooserActionOpen Nothing >>= maybe (return ()) action 
  where 
    action filename = do 
      xst <- get 
      bstr <- liftIO $ B.readFile filename 
      let uhdl = view (unitHoodles.currentUnit) xst
          cid = getCurrentCanvasId uhdl
          pgnum = view (currentCanvasInfo . unboxLens currentPageNum) uhdl
          hdl = getHoodle uhdl
          currpage = getPageFromGHoodleMap pgnum hdl
          currlayer = getCurrentLayer currpage
      --
      callRenderer $ return . GotRItem =<< (cnstrctRItem . ItemSVG) 
                       (SVG Nothing Nothing bstr (100,100) (Dim 300 300))
      RenderEv (GotRItem newitem) <- waitSomeEvent (\case RenderEv (GotRItem _) -> True ; _ -> False )
      -- 
      let otheritems = view gitems currlayer  
      let ntpg = makePageSelectMode currpage (otheritems :- (Hitted [newitem]) :- Empty)  
      modeChange ToSelectMode 
      updateUhdl $ \uhdl' -> do
        thdl <- case view hoodleModeState uhdl' of
                  SelectState thdl' -> return thdl'
                  _ -> (lift . EitherT . return . Left . Other) "fileLoadSVG"
        nthdl <- updateTempHoodleSelectM cid thdl ntpg pgnum 
        return . (hoodleModeState .~ SelectState nthdl)
               . (isOneTimeSelectMode .~ YesAfterSelect) $ uhdl'
      commit_
      invalidateAll 

-- |
askQuitProgram :: MainCoroutine () 
askQuitProgram = do 
    b <- okCancelMessageBox "Current canvas is not saved yet. Will you close hoodle?" 
    case b of 
      True -> doIOaction_ $ Gtk.postGUIAsync Gtk.mainQuit >> return (UsrEv ActionOrdered)
      False -> return ()
  
-- | 
embedPredefinedImage :: MainCoroutine () 
embedPredefinedImage = do 
    mpredefined <- S.embedPredefinedImageHook 
    case mpredefined of 
      Nothing -> return () 
      Just filename -> embedImage filename
          
-- | this is temporary. I will remove it
embedPredefinedImage2 :: MainCoroutine () 
embedPredefinedImage2 = do 
    mpredefined <- S.embedPredefinedImage2Hook 
    case mpredefined of 
      Nothing -> return () 
      Just filename -> embedImage filename 
        
-- | this is temporary. I will remove it
embedPredefinedImage3 :: MainCoroutine () 
embedPredefinedImage3 = do 
    mpredefined <- S.embedPredefinedImage3Hook 
    case mpredefined of 
      Nothing -> return () 
      Just filename -> embedImage filename 
        
-- | 
embedAllPDFBackground :: MainCoroutine () 
embedAllPDFBackground = do 
  hdl <- (rHoodle2Hoodle  . getHoodle . view (unitHoodles.currentUnit)) <$> get
  nhdl <- liftIO . embedPDFInHoodle $ hdl
  constructNewHoodleStateFromHoodle nhdl
  commit_
  invalidateAll   
  
-- | embed an item from hoodlet using hoodlet identifier
embedHoodlet :: String -> MainCoroutine ()
embedHoodlet str = loadHoodlet str >>= mapM_ (insertItemAt Nothing) 

-- |
mkRevisionHdlFile :: Hoodle -> IO (String,String)
mkRevisionHdlFile hdl = do 
    hdir <- getHomeDirectory
    tempfile <- mkTmpFile "hdl"
    let hdlbstr = builder hdl 
    L.writeFile tempfile hdlbstr
    ctime <- getCurrentTime 
    let idstr = B.unpack (view hoodleID hdl)
        md5str = show (md5 hdlbstr)
        name = "UUID_"++idstr++"_MD5Digest_"++md5str++"_ModTime_"++ show ctime
        nfilename = name <.> "hdl"
        vcsdir = hdir </> ".hoodle.d" </> "vcs"
    b <- doesDirectoryExist vcsdir 
    unless b $ createDirectory vcsdir
    renameFile tempfile (vcsdir </> nfilename)  
    return (md5str,name) 


mkRevisionPdfFile :: Hoodle -> String -> IO ()
mkRevisionPdfFile hdl fname = do 
    hdir <- getHomeDirectory
    tempfile <- mkTmpFile "pdf"
    renderHoodleToPDF hdl tempfile 
    let nfilename = fname <.> "pdf"
        vcsdir = hdir </> ".hoodle.d" </> "vcs"
    b <- doesDirectoryExist vcsdir 
    unless b $ createDirectory vcsdir
    renameFile tempfile (vcsdir </> nfilename)  

-- | 
fileVersionSave :: MainCoroutine () 
fileVersionSave = do 
    hdl <- rHoodle2Hoodle . getHoodle . view (unitHoodles.currentUnit) <$> get
    rmini <- minibufDialog "Commit Message:"
    case rmini of 
      Right [] -> return ()
      Right strks' -> do
        doIOaction $ \_evhandler -> do 
          (md5str,fname) <- mkRevisionHdlFile hdl
          mkRevisionPdfFile hdl fname
          return (UsrEv (GotRevisionInk md5str strks'))
        r <- waitSomeEvent (\case GotRevisionInk _ _ -> True ; _ -> False )
        let GotRevisionInk md5str strks = r          
            nrev = RevisionInk (B.pack md5str) strks
        pureUpdateUhdl $ \uhdl -> 
          let hdlmodst = view hoodleModeState uhdl
          in case hdlmodst of 
               ViewAppendState rhdl' -> 
                 let nrhdl = over grevisions (<> [nrev]) rhdl' 
                 in (hoodleModeState .~ ViewAppendState nrhdl) uhdl
               SelectState thdl -> 
                 let nthdl = over gselRevisions (<> [nrev]) thdl
                 in (hoodleModeState .~ SelectState nthdl) uhdl
        commit_ 
      Left () -> do 
        txtstr <- maybe "" id <$> textInputDialog "revision description"
        doIOaction $ \_evhandler -> do 
          (md5str,fname) <- mkRevisionHdlFile hdl
          mkRevisionPdfFile hdl fname
          return (UsrEv (GotRevision md5str txtstr))
        r <- waitSomeEvent (\case GotRevision _ _ -> True ; _ -> False )
        let GotRevision md5str txtstr' = r          
            nrev = Revision (B.pack md5str) (B.pack txtstr')
        pureUpdateUhdl $ \uhdl -> 
          let hdlmodst = view hoodleModeState uhdl
          in case hdlmodst of 
               ViewAppendState rhdl' -> 
                 let nrhdl = over grevisions (<> [nrev]) rhdl' 
                 in (hoodleModeState .~ ViewAppendState nrhdl) uhdl
               SelectState thdl -> 
                 let nthdl = over gselRevisions (<> [nrev]) thdl
                 in (hoodleModeState .~ SelectState nthdl) uhdl
        commit_ 



showRevisionDialog :: Hoodle -> [Revision] -> MainCoroutine ()
showRevisionDialog hdl revs = do
    cid <- getCurrentCanvasId . view (unitHoodles.currentUnit) <$> get
    cache <- renderCache
    doIOaction (action (cache,cid))
    waitSomeEvent (\case GotOk -> True ; _ -> False)
    return ()
  where 
    action (cache,cid) _evhandler = do 
      dialog <- Gtk.dialogNew
#ifdef GTK3
      upper <- fmap Gtk.castToContainer (Gtk.dialogGetContentArea dialog)
      vbox <- Gtk.vBoxNew False 0
      Gtk.containerAdd upper vbox 
#else 
      vbox <- Gtk.dialogGetUpper dialog
#endif
      mapM_ (addOneRevisionBox cache cid vbox hdl) revs 
      _btnOk <- Gtk.dialogAddButton dialog ("Ok" :: String) Gtk.ResponseOk
      Gtk.widgetShowAll dialog
      _res <- Gtk.dialogRun dialog
      Gtk.widgetDestroy dialog
      return (UsrEv GotOk)


mkPangoText :: String -> Cairo.Render ()
mkPangoText str = do 
    let pangordr = do 
          ctxt <- Gtk.cairoCreateContext Nothing 
          layout <- Gtk.layoutEmpty ctxt   
          fdesc <- Gtk.fontDescriptionNew 
          Gtk.fontDescriptionSetFamily fdesc ("Sans Mono" :: String)
          Gtk.fontDescriptionSetSize fdesc 8.0 
          Gtk.layoutSetFontDescription layout (Just fdesc)
          Gtk.layoutSetWidth layout (Just 250)
          Gtk.layoutSetWrap layout Gtk.WrapAnywhere 
          Gtk.layoutSetText layout str 
          return layout
        rdr layout = do Cairo.setSourceRGBA 0 0 0 1
                        Gtk.updateLayout layout 
                        Gtk.showLayout layout 
    layout <- liftIO $ pangordr 
    rdr layout

addOneRevisionBox :: RenderCache -> CanvasId -> Gtk.VBox -> Hoodle -> Revision -> IO ()
addOneRevisionBox cache cid vbox hdl rev = do 
    cvs <- Gtk.drawingAreaNew 
    cvs `Gtk.on` Gtk.sizeRequest $ return (Gtk.Requisition 250 25)
    cvs `Gtk.on` Gtk.exposeEvent $ Gtk.tryEvent $ do 
#ifdef GTK3      
      Just drawwdw <- liftIO $ Gtk.widgetGetWindow cvs 
#else
      drawwdw <- liftIO $ Gtk.widgetGetDrawWindow cvs 
#endif
#ifdef GTK3
      liftIO . Gtk.renderWithDrawWindow drawwdw $ do
#else 
      liftIO . Gtk.renderWithDrawable drawwdw $ do 
#endif
        case rev of 
          RevisionInk _ strks -> Cairo.scale 0.5 0.5 >> mapM_ (cairoRender cache cid) strks
          Revision _ txt -> mkPangoText (B.unpack txt)            
    hdir <- getHomeDirectory
    let vcsdir = hdir </> ".hoodle.d" </> "vcs"
    btn <- Gtk.buttonNewWithLabel ("view" :: String)
    btn `Gtk.on` Gtk.buttonPressEvent $ Gtk.tryEvent $ do 
      files <- liftIO $ getDirectoryContents vcsdir 
      let fstrinit = "UUID_" ++ B.unpack (view hoodleID hdl)  
                      ++ "_MD5Digest_" ++ B.unpack (view revmd5 rev)
                 
          matched = filter ((== "fdp") . take 3 . reverse) 
                    . filter (\f -> fstrinit  `List.isPrefixOf` f) $ files
      case matched of 
        x : _ -> 
          liftIO (createProcess (proc "evince" [vcsdir </> x])) 
          >> return ()
        _ -> return ()    
    hbox <- Gtk.hBoxNew False 0
    Gtk.boxPackStart hbox cvs Gtk.PackNatural 0
    Gtk.boxPackStart hbox btn Gtk.PackGrow  0
    Gtk.boxPackStart vbox hbox Gtk.PackNatural 0

fileShowRevisions :: MainCoroutine ()
fileShowRevisions = do 
    rhdl <- getHoodle . view (unitHoodles.currentUnit) <$> get  
    let hdl = rHoodle2Hoodle rhdl
    let revs = view grevisions rhdl
    showRevisionDialog hdl revs 
  
fileShowUUID :: MainCoroutine ()
fileShowUUID = do 
    hdl <- getHoodle . view (unitHoodles.currentUnit) <$> get  
    let uuidstr = view ghoodleID hdl
    okMessageBox (B.unpack uuidstr)
  

loadHoodlet :: String -> MainCoroutine (Maybe RItem)
loadHoodlet str = do
     homedir <- liftIO getHomeDirectory
     let hoodled = homedir </> ".hoodle.d"
         hoodletdir = hoodled </> "hoodlet"
     b' <- liftIO $ doesDirectoryExist hoodletdir 
     if b' 
       then do            
         let fp = hoodletdir </> str <.> "hdlt"
         bstr <- liftIO $ B.readFile fp 
         case parseOnly Hoodlet.hoodlet bstr of 
           Left err -> msgShout err >> return Nothing
           Right itm -> do
             --
             callRenderer $ cnstrctRItem itm >>= return . GotRItem 
             RenderEv (GotRItem ritm) <- 
               waitSomeEvent (\case RenderEv (GotRItem _) -> True; _ -> False )
             --
             return (Just ritm) 
       else return Nothing

  
  
  
