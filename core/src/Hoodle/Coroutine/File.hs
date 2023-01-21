{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-matches -fno-warn-incomplete-uni-patterns #-}

module Hoodle.Coroutine.File where

import Control.Lens (at, over, set, view, (.~), (?~))
import Control.Monad (guard, unless, void, when, (<=<))
import Control.Monad.State (get, gets, lift, liftIO)
import Control.Monad.Trans.Crtn (CrtnErr (Other))
import Control.Monad.Trans.Except (ExceptT (..))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.ByteString.Char8 as B (pack, readFile, unpack)
import qualified Data.ByteString.Lazy as L
import Data.Digest.Pure.MD5 (md5)
import Data.Foldable (forM_, mapM_)
import Data.Functor ((<&>))
import Data.Hoodle.Generic
  ( gdimension,
    ghoodleID,
    gitems,
    gpages,
    grevisions,
    gtitle,
  )
import Data.Hoodle.Select
  ( gselRevisions,
    gselTitle,
  )
import Data.Hoodle.Simple
  ( Dimension (Dim),
    Hoodle,
    Image (..),
    Item (..),
    Revision (..),
    SVG (..),
    defaultHoodle,
    hoodleID,
    revmd5,
    title,
  )
import qualified Data.List as List
import Data.Maybe (fromJust, fromMaybe, maybeToList)
import Data.Time.Clock (getCurrentTime)
import Graphics.Hoodle.Render (Xform4Page (..), cnstrctRHoodle)
import Graphics.Hoodle.Render.Generic
  ( cairoRender,
    cairoRenderOption,
  )
import Graphics.Hoodle.Render.Item (cnstrctRItem)
import Graphics.Hoodle.Render.Type
  ( InBBox (..),
    InBBoxOption (..),
    RItem,
    RenderCache,
    rHoodle2Hoodle,
  )
import Graphics.Hoodle.Render.Type.HitTest (AlterList (..), Hitted (..))
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.UI.Gtk as Gtk
import Hoodle.Accessor
  ( applyActionToAllCVS,
    getCurrentPageCurr,
    getHoodleFilePath,
    pureUpdateUhdl,
    renderCache,
    updateUhdl,
  )
import Hoodle.Coroutine.Commit
  ( clearUndoHistory,
    commit_,
  )
import Hoodle.Coroutine.Dialog
  ( fileChooser,
    okCancelMessageBox,
    okMessageBox,
    textInputDialog,
  )
import Hoodle.Coroutine.Draw
  ( callRenderer,
    callRenderer_,
    doIOaction_,
    invalidateAll,
    waitSomeEvent,
  )
import Hoodle.Coroutine.Layer
  ( makeNewLayer,
  )
import Hoodle.Coroutine.Minibuffer
  ( minibufDialog,
  )
import Hoodle.Coroutine.Mode (modeChange)
import Hoodle.Coroutine.Page
  ( canvasZoomUpdateAll,
    changePage,
    newPage,
  )
import Hoodle.Coroutine.Scroll (adjustScrollbarWithGeometryCvsId)
import Hoodle.Coroutine.Select.Clipboard (updateTempHoodleSelectM)
import Hoodle.Coroutine.TextInput
  ( autoPosText,
    insertItemAt,
  )
import Hoodle.GUI.Reflect (reflectUIToggle)
import Hoodle.ModelAction.File
  ( checkVersionAndMigrate,
    embedPDFInHoodle,
    makeNewHoodleWithPDF,
    makeNewItemImage,
    saveHoodle,
  )
import Hoodle.ModelAction.Layer (getCurrentLayer)
import Hoodle.ModelAction.Page
  ( getPageFromGHoodleMap,
    setPage,
  )
import Hoodle.ModelAction.Select (makePageSelectMode)
import Hoodle.ModelAction.Window (setTitleFromFileName)
-- import Hoodle.Publish.PDF (renderHoodleToPDF)
import qualified Hoodle.Script.Coroutine as S
import Hoodle.Script.Hook
  ( fileNameSuggestionHook,
    saveAsHook,
    shrinkCmd4EmbedImage,
    warningEmbedImageSize,
  )
import Hoodle.Type.Canvas
  ( CanvasId,
    currentPageNum,
    unboxLens,
  )
import Hoodle.Type.Coroutine (MainCoroutine, doIOaction)
import Hoodle.Type.Enum (AddDirection (PageBefore))
import Hoodle.Type.Event
  ( AllEvent (UsrEv),
    RenderEvent
      ( GotRHoodle,
        GotRItem
      ),
    UserEvent
      ( ActionOrdered,
        GotOk,
        GotRevision,
        GotRevisionInk,
        RenderEv,
        ToSelectMode,
        ToViewAppendMode
      ),
  )
import Hoodle.Type.HoodleState
  ( FileStore (LocalDir),
    HoodleModeState (SelectState, ViewAppendState),
    IsOneTimeSelectMode (YesAfterSelect),
    currentCanvasInfo,
    currentUnit,
    doesEmbedImage,
    doesEmbedPDF,
    getCurrentCanvasId,
    getHoodle,
    gtkUIManager,
    hoodleFileControl,
    hoodleFileName,
    hoodleModeState,
    hookSet,
    isOneTimeSelectMode,
    isSaved,
    lastSavedTime,
    resetHoodleModeStateBuffers,
    settings,
    syncMD5History,
    unitHoodles,
  )
import Hoodle.Type.PageArrangement (PageCoordinate (..), PageNum (..))
import Hoodle.Util (mkTmpFile, msgShout)
import System.Directory
  ( createDirectory,
    doesDirectoryExist,
    doesFileExist,
    getDirectoryContents,
    getHomeDirectory,
    renameFile,
  )
import System.FilePath
  ( splitFileName,
    takeExtension,
    (<.>),
    (</>),
  )
import System.IO (IOMode (..), hClose, hFileSize, openFile)
import System.Process
  ( createProcess,
    proc,
  )
import Text.Hoodle.Builder (builder)
import Text.Hoodle.Migrate.FromXournal (mkHoodleFromXournal)
import qualified Text.Hoodlet.Parse.Attoparsec as Hoodlet
import qualified Text.Xournal.Parse.Conduit as XP
--
import Prelude hiding (concat, mapM, mapM_, readFile)

-- |
askIfSave :: MainCoroutine () -> MainCoroutine ()
askIfSave action = do
  uhdl <- gets (view (unitHoodles . currentUnit))
  if not (view isSaved uhdl)
    then okCancelMessageBox "Current canvas is not saved yet. Will you proceed without save?" >>= flip when action
    else action

-- |
askIfOverwrite :: FilePath -> MainCoroutine () -> MainCoroutine ()
askIfOverwrite fp action = do
  b <- liftIO $ doesFileExist fp
  if b
    then do
      r <- okCancelMessageBox ("Overwrite " ++ fp ++ "???")
      when r action
    else action

-- | get file content from xournal file and update hoodle state
getFileContent :: FileStore -> MainCoroutine ()
getFileContent store@(LocalDir (Just fname)) = do
  xstate <- get
  let ext = takeExtension fname
  case ext of
    ".hdl" -> do
      liftIO $ putStrLn "file1"
      bstr <- liftIO $ B.readFile fname
      liftIO $ putStrLn "file2"
      liftIO $ print fname
      liftIO $ print bstr
      r <- liftIO $ checkVersionAndMigrate bstr
      liftIO $ putStrLn "file3"
      case r of
        Left err -> liftIO $ putStrLn err
        Right h -> do
          liftIO $ putStrLn "file4"
          constructNewHoodleStateFromHoodle h
          liftIO $ putStrLn "file5"
          ctime <- liftIO getCurrentTime
          liftIO $ putStrLn "file6"
          let mmd5 = Nothing
          pureUpdateUhdl
            ( (hoodleFileControl . hoodleFileName .~ store)
                . (hoodleFileControl . lastSavedTime ?~ ctime)
                . (hoodleFileControl . syncMD5History .~ maybeToList mmd5)
            )
          liftIO $ putStrLn "file7"
          commit_
    ".xoj" -> do
      liftIO (XP.parseXojFile fname) >>= \case
        Left str -> msgShout $ "file reading error : " ++ str
        Right xojcontent -> do
          hdlcontent <- liftIO $ mkHoodleFromXournal xojcontent
          constructNewHoodleStateFromHoodle hdlcontent
          ctime <- liftIO getCurrentTime
          pureUpdateUhdl
            ( (hoodleFileControl . hoodleFileName .~ LocalDir Nothing)
                . (hoodleFileControl . lastSavedTime ?~ ctime)
            )
          commit_
    ".pdf" -> do
      let doesembed = view (settings . doesEmbedPDF) xstate
      mhdl <- liftIO $ makeNewHoodleWithPDF doesembed fname
      case mhdl of
        Nothing -> getFileContent (LocalDir Nothing)
        Just hdl -> do
          constructNewHoodleStateFromHoodle hdl
          pureUpdateUhdl (hoodleFileControl . hoodleFileName .~ LocalDir Nothing)
          commit_
    _ -> getFileContent (LocalDir Nothing)
  xstate' <- get
  doIOaction_ $ Gtk.postGUIAsync (setTitleFromFileName xstate')
getFileContent (LocalDir Nothing) = do
  constructNewHoodleStateFromHoodle =<< liftIO defaultHoodle
  pureUpdateUhdl (hoodleFileControl . hoodleFileName .~ LocalDir Nothing)
  commit_
getFileContent _ = return ()

-- |
constructNewHoodleStateFromHoodle :: Hoodle -> MainCoroutine ()
constructNewHoodleStateFromHoodle hdl' = do
  callRenderer $
    cnstrctRHoodle hdl' <&> GotRHoodle
  RenderEv (GotRHoodle rhdl) <- waitSomeEvent (\case RenderEv (GotRHoodle _) -> True; _ -> False)
  pureUpdateUhdl (hoodleModeState .~ ViewAppendState rhdl)

-- | deprecated
fileNew :: MainCoroutine ()
fileNew = do
  getFileContent (LocalDir Nothing)
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
  uhdl <- gets (view (unitHoodles . currentUnit))
  case getHoodleFilePath uhdl of
    Nothing -> fileSaveAs
    Just filename -> do
      -- this is rather temporary not to make mistake
      if takeExtension filename == ".hdl"
        then do
          updateUhdl $ liftIO . saveHoodle . const uhdl
          (S.afterSaveHook filename . rHoodle2Hoodle . getHoodle) uhdl
        else fileExtensionInvalid (".hdl", "save") >> fileSaveAs

-- | interleaving a monadic action between each pair of subsequent actions
sequence1_ :: (Monad m) => m () -> [m ()] -> m ()
sequence1_ _ [] = return ()
sequence1_ _ [a] = a
sequence1_ i (a : as) = a >> i >> sequence1_ i as

-- |
fileExport :: MainCoroutine ()
fileExport = fileChooser Gtk.FileChooserActionSave Nothing >>= maybe (return ()) action
  where
    action filename = do
      -- this is rather temporary not to make mistake
      if takeExtension filename /= ".pdf"
        then fileExtensionInvalid (".pdf", "export") >> fileExport
        else do
          hdl <- gets (rHoodle2Hoodle . getHoodle . view (unitHoodles . currentUnit))
          -- liftIO (renderHoodleToPDF hdl filename)
          liftIO $ putStrLn "renderHoodleToPDF is disabled for now."

-- | need to be merged with ContextMenuEventSVG
exportCurrentPageAsSVG :: MainCoroutine ()
exportCurrentPageAsSVG = fileChooser Gtk.FileChooserActionSave Nothing >>= maybe (return ()) action
  where
    action filename =
      -- this is rather temporary not to make mistake
      if takeExtension filename /= ".svg"
        then fileExtensionInvalid (".svg", "export") >> exportCurrentPageAsSVG
        else do
          cvsid <- gets (getCurrentCanvasId . view (unitHoodles . currentUnit))
          cache <- renderCache
          cpg <- getCurrentPageCurr
          let Dim w h = view gdimension cpg
          liftIO $
            Cairo.withSVGSurface filename w h $ \s ->
              Cairo.renderWith s $
                void $
                  cairoRenderOption
                    (InBBoxOption Nothing)
                    cache
                    cvsid
                    (InBBox cpg, Nothing :: Maybe Xform4Page)

-- |
fileLoad :: FileStore -> MainCoroutine ()
fileLoad filestore = do
  liftIO $ putStrLn "load1"
  getFileContent filestore
  liftIO $ putStrLn "load2"
  updateUhdl $ \uhdl -> do
    ncvsinfo <- liftIO $ setPage uhdl 0 (getCurrentCanvasId uhdl)
    return . (currentCanvasInfo .~ ncvsinfo) . (isSaved .~ True) $ uhdl
  liftIO $ putStrLn "load3"
  xst <- get
  liftIO $ putStrLn "load4"
  let ui = view gtkUIManager xst
  liftIO $ reflectUIToggle ui "SAVEA" False
  liftIO $ putStrLn "load5"
  liftIO $ setTitleFromFileName xst
  liftIO $ putStrLn "load6"
  clearUndoHistory
  liftIO $ putStrLn "load7"
  modeChange ToViewAppendMode
  liftIO $ putStrLn "load8"
  canvasZoomUpdateAll
  liftIO $ putStrLn "load9"
  resetHoodleBuffers
  liftIO $ putStrLn "load10"
  invalidateAll
  liftIO $ putStrLn "load11"
  applyActionToAllCVS adjustScrollbarWithGeometryCvsId

-- |
resetHoodleBuffers :: MainCoroutine ()
resetHoodleBuffers = do
  updateUhdl $ \uhdl -> do
    let hdlst = view hoodleModeState uhdl
        cvsid = getCurrentCanvasId uhdl
    callRenderer_ $ resetHoodleModeStateBuffers cvsid hdlst
    return . (hoodleModeState .~ hdlst) $ uhdl

-- | main coroutine for open a file
fileOpen :: MainCoroutine ()
fileOpen = do
  mfilename <- fileChooser Gtk.FileChooserActionOpen Nothing
  forM_ mfilename (fileLoad . LocalDir . Just)

-- | main coroutine for save as
fileSaveAs :: MainCoroutine ()
fileSaveAs = do
  hdl <- gets (rHoodle2Hoodle . getHoodle . view (unitHoodles . currentUnit))
  maybe (defSaveAsAction hdl) (\f -> liftIO (f hdl)) =<< hookSaveAsAction
  where
    hookSaveAsAction = gets (saveAsHook <=< view hookSet)
    msuggestedact = gets (fileNameSuggestionHook <=< view hookSet)
    defSaveAsAction hdl = do
      (msuggested :: Maybe String) <- maybe (return Nothing) (fmap Just . liftIO) =<< msuggestedact
      mr <- fileChooser Gtk.FileChooserActionSave msuggested
      maybe (return ()) (action hdl) mr
      where
        action hd filename =
          if takeExtension filename /= ".hdl"
            then fileExtensionInvalid (".hdl", "save")
            else do
              askIfOverwrite filename $ do
                updateUhdl $ \uhdl -> do
                  let ntitle = B.pack . snd . splitFileName $ filename
                      (hdlmodst', hdl') = case view hoodleModeState uhdl of
                        ViewAppendState hdlmap ->
                          if view gtitle hdlmap == "untitled"
                            then (ViewAppendState (set gtitle ntitle hdlmap), set title ntitle hd)
                            else (ViewAppendState hdlmap, hd)
                        SelectState thdl ->
                          if view gselTitle thdl == "untitled"
                            then
                              ( SelectState $ set gselTitle ntitle thdl,
                                set title ntitle hd
                              )
                            else (SelectState thdl, hd)
                  liftIO . L.writeFile filename . builder $ hdl'
                  return . (hoodleFileControl . hoodleFileName .~ LocalDir (Just filename))
                    . (hoodleModeState .~ hdlmodst')
                    . (isSaved .~ True)
                    $ uhdl
              xst <- get
              let ui = view gtkUIManager xst
                  hdl'' = (rHoodle2Hoodle . getHoodle . view (unitHoodles . currentUnit)) xst
              liftIO $ reflectUIToggle ui "SAVEA" False
              liftIO $ setTitleFromFileName xst
              S.afterSaveHook filename hdl''

-- | main coroutine for open a file
fileReload :: MainCoroutine ()
fileReload = do
  liftIO $ putStrLn "here1"
  uhdl <- gets (view (unitHoodles . currentUnit))
  liftIO $ putStrLn "here2"
  let filestore = view (hoodleFileControl . hoodleFileName) uhdl
  if not (view isSaved uhdl)
    then do
      liftIO $ putStrLn "here3"
      b <- okCancelMessageBox "Discard changes and reload the file?"
      when b (fileLoad filestore)
    else do
      liftIO $ putStrLn "here4"
      fileLoad filestore

-- |
fileExtensionInvalid :: (String, String) -> MainCoroutine ()
fileExtensionInvalid (ext, a) =
  okMessageBox $
    "only "
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
      let doesembed = view (settings . doesEmbedPDF) xstate
      mhdl <- liftIO $ makeNewHoodleWithPDF doesembed filename
      flip (maybe warning) mhdl $ \hdl -> do
        constructNewHoodleStateFromHoodle hdl
        pureUpdateUhdl (hoodleFileControl . hoodleFileName .~ LocalDir Nothing)
        commit_
        setTitleFromFileName_
        canvasZoomUpdateAll

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
    let ext =
          let x' = takeExtension filename
           in if (not . null) x' then tail x' else ""
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
      let fDoesEmbedImg = view (settings . doesEmbedImage) xst
          uhdl = view (unitHoodles . currentUnit) xst
          hdl = getHoodle uhdl
          Dim pw _ph = (view gdimension . fromJust . view (gpages . at 0)) hdl
      itm <-
        if fDoesEmbedImg
          then
            checkEmbedImageSize filename
              >>= maybe
                (liftIO $ makeNewItemImage True filename)
                (liftIO . makeNewItemImage True)
          else liftIO (makeNewItemImage False filename)
      let ItemImage img = itm
          Dim w h = img_dim img
          ratio = h / w
          ndim = Dim pw (pw * ratio)
          img' = img {img_dim = ndim}
      changePage (const 0)
      newPage (Just ndim) PageBefore
      callRenderer $
        cnstrctRItem (ItemImage img') <&> GotRItem
      RenderEv (GotRItem nitm) <- waitSomeEvent (\case RenderEv (GotRItem _) -> True; _ -> False)
      insertItemAt (Just (PageNum 0, PageCoord (0, 0))) nitm
      modeChange ToViewAppendMode
      makeNewLayer

embedImage :: FilePath -> MainCoroutine ()
embedImage filename = do
  xst <- get
  let fDoesEmbedImg = view (settings . doesEmbedImage) xst
      uhdl = view (unitHoodles . currentUnit) xst
  nitm <-
    if fDoesEmbedImg
      then do
        mf <- checkEmbedImageSize filename
        --
        callRenderer $ case mf of
          Nothing ->
            (liftIO (makeNewItemImage True filename) >>= cnstrctRItem) <&> GotRItem
          Just f ->
            (liftIO (makeNewItemImage True f) >>= cnstrctRItem) <&> GotRItem
        RenderEv (GotRItem r) <- waitSomeEvent (\case RenderEv (GotRItem _) -> True; _ -> False)
        return r
      else do
        callRenderer $
          (liftIO (makeNewItemImage False filename) >>= cnstrctRItem) <&> GotRItem
        RenderEv (GotRItem r) <- waitSomeEvent (\case RenderEv (GotRItem _) -> True; _ -> False)
        return r
  let cpn = view (currentCanvasInfo . unboxLens currentPageNum) uhdl
  my <- autoPosText
  let mpos = (\y -> (PageNum cpn, PageCoord (50, y))) <$> my
  insertItemAt mpos nitm

-- |
fileLoadSVG :: MainCoroutine ()
fileLoadSVG = do
  fileChooser Gtk.FileChooserActionOpen Nothing >>= maybe (return ()) action
  where
    action filename = do
      xst <- get
      bstr <- liftIO $ B.readFile filename
      let uhdl = view (unitHoodles . currentUnit) xst
          cvsid = getCurrentCanvasId uhdl
          pgnum = view (currentCanvasInfo . unboxLens currentPageNum) uhdl
          hdl = getHoodle uhdl
          currpage = getPageFromGHoodleMap pgnum hdl
          currlayer = getCurrentLayer currpage
      --
      callRenderer $
        GotRItem
          <$> (cnstrctRItem . ItemSVG)
            (SVG Nothing Nothing bstr (100, 100) (Dim 300 300))
      RenderEv (GotRItem newitem) <- waitSomeEvent (\case RenderEv (GotRItem _) -> True; _ -> False)
      --
      let otheritems = view gitems currlayer
      let ntpg = makePageSelectMode currpage (otheritems :- Hitted [newitem] :- Empty)
      modeChange ToSelectMode
      updateUhdl $ \uhdl' -> do
        thdl <- case view hoodleModeState uhdl' of
          SelectState thdl' -> return thdl'
          _ -> (lift . ExceptT . return . Left . Other) "fileLoadSVG"
        nthdl <- updateTempHoodleSelectM cvsid thdl ntpg pgnum
        return . (hoodleModeState .~ SelectState nthdl)
          . (isOneTimeSelectMode .~ YesAfterSelect)
          $ uhdl'
      commit_
      invalidateAll

-- |
askQuitProgram :: MainCoroutine ()
askQuitProgram = do
  b <- okCancelMessageBox "Current canvas is not saved yet. Will you close hoodle?"
  when b $
    doIOaction_ $
      Gtk.postGUIAsync Gtk.mainQuit >> return (UsrEv ActionOrdered)

-- |
embedPredefinedImage :: MainCoroutine ()
embedPredefinedImage = do
  mpredefined <- S.embedPredefinedImageHook
  forM_ mpredefined embedImage

-- | this is temporary. I will remove it
embedPredefinedImage2 :: MainCoroutine ()
embedPredefinedImage2 = do
  mpredefined <- S.embedPredefinedImage2Hook
  forM_ mpredefined embedImage

-- | this is temporary. I will remove it
embedPredefinedImage3 :: MainCoroutine ()
embedPredefinedImage3 = do
  mpredefined <- S.embedPredefinedImage3Hook
  forM_ mpredefined embedImage

-- |
embedAllPDFBackground :: MainCoroutine ()
embedAllPDFBackground = do
  hdl <- gets (rHoodle2Hoodle . getHoodle . view (unitHoodles . currentUnit))
  nhdl <- liftIO . embedPDFInHoodle $ hdl
  constructNewHoodleStateFromHoodle nhdl
  commit_
  invalidateAll

-- | embed an item from hoodlet using hoodlet identifier
embedHoodlet :: String -> MainCoroutine ()
embedHoodlet str = loadHoodlet str >>= mapM_ (insertItemAt Nothing)

-- |
mkRevisionHdlFile :: Hoodle -> IO (String, String)
mkRevisionHdlFile hdl = do
  hdir <- getHomeDirectory
  tempfile <- mkTmpFile "hdl"
  let hdlbstr = builder hdl
  L.writeFile tempfile hdlbstr
  ctime <- getCurrentTime
  let idstr = B.unpack (view hoodleID hdl)
      md5str = show (md5 hdlbstr)
      name = "UUID_" ++ idstr ++ "_MD5Digest_" ++ md5str ++ "_ModTime_" ++ show ctime
      nfilename = name <.> "hdl"
      vcsdir = hdir </> ".hoodle.d" </> "vcs"
  b <- doesDirectoryExist vcsdir
  unless b $ createDirectory vcsdir
  renameFile tempfile (vcsdir </> nfilename)
  return (md5str, name)

mkRevisionPdfFile :: Hoodle -> String -> IO ()
mkRevisionPdfFile hdl fname = do
  putStrLn "mkRevisionPdfFile is disabled for now"

{-  hdir <- getHomeDirectory
  tempfile <- mkTmpFile "pdf"
  renderHoodleToPDF hdl tempfile
  let nfilename = fname <.> "pdf"
      vcsdir = hdir </> ".hoodle.d" </> "vcs"
  b <- doesDirectoryExist vcsdir
  unless b $ createDirectory vcsdir
  renameFile tempfile (vcsdir </> nfilename)
-}

-- |
fileVersionSave :: MainCoroutine ()
fileVersionSave = do
  hdl <- gets (rHoodle2Hoodle . getHoodle . view (unitHoodles . currentUnit))
  rmini <- minibufDialog "Commit Message:"
  case rmini of
    Right [] -> return ()
    Right strks' -> do
      doIOaction $ \_evhandler -> do
        (md5str, fname) <- mkRevisionHdlFile hdl
        mkRevisionPdfFile hdl fname
        return (UsrEv (GotRevisionInk md5str strks'))
      r <- waitSomeEvent (\case GotRevisionInk _ _ -> True; _ -> False)
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
      txtstr <- fromMaybe "" <$> textInputDialog "revision description"
      doIOaction $ \_evhandler -> do
        (md5str, fname) <- mkRevisionHdlFile hdl
        mkRevisionPdfFile hdl fname
        return (UsrEv (GotRevision md5str txtstr))
      r <- waitSomeEvent (\case GotRevision _ _ -> True; _ -> False)
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
  cvsid <- gets (getCurrentCanvasId . view (unitHoodles . currentUnit))
  cache <- renderCache
  doIOaction (action (cache, cvsid))
  void $ waitSomeEvent (\case GotOk -> True; _ -> False)
  where
    action (cache, cvsid) _evhandler = do
      dialog <- Gtk.dialogNew
      upper <- fmap Gtk.castToContainer (Gtk.dialogGetContentArea dialog)
      vbox <- Gtk.vBoxNew False 0
      Gtk.containerAdd upper vbox
      mapM_ (addOneRevisionBox cache cvsid vbox hdl) revs
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
      rdr layout = do
        Cairo.setSourceRGBA 0 0 0 1
        Gtk.updateLayout layout
        Gtk.showLayout layout
  layout <- liftIO pangordr
  rdr layout

addOneRevisionBox :: RenderCache -> CanvasId -> Gtk.VBox -> Hoodle -> Revision -> IO ()
addOneRevisionBox cache cvsid vbox hdl rev = do
  cvs <- Gtk.drawingAreaNew
  Gtk.widgetSetSizeRequest cvs 250 25
  _ <- cvs `Gtk.on` Gtk.draw $ do
    -- TODO: make a safe version
    drawwdw <-
      liftIO (Gtk.widgetGetWindow cvs) >>= \case
        Just drawwdw -> pure drawwdw
        _ -> error "addOneRevisionBox: no canvas window"
    liftIO . Gtk.renderWithDrawWindow drawwdw $ do
      case rev of
        RevisionInk _ strks -> Cairo.scale 0.5 0.5 >> mapM_ (cairoRender cache cvsid) strks
        Revision _ txt -> mkPangoText (B.unpack txt)
  hdir <- getHomeDirectory
  let vcsdir = hdir </> ".hoodle.d" </> "vcs"
  btn <- Gtk.buttonNewWithLabel ("view" :: String)
  _ <- btn `Gtk.on` Gtk.buttonPressEvent $
    Gtk.tryEvent $ do
      files <- liftIO $ getDirectoryContents vcsdir
      let fstrinit =
            "UUID_" ++ B.unpack (view hoodleID hdl)
              ++ "_MD5Digest_"
              ++ B.unpack (view revmd5 rev)
          matched =
            filter ((== "fdp") . take 3 . reverse)
              . filter (\f -> fstrinit `List.isPrefixOf` f)
              $ files
      case matched of
        x : _ -> void $ liftIO (createProcess (proc "evince" [vcsdir </> x]))
        _ -> return ()
  hbox <- Gtk.hBoxNew False 0
  Gtk.boxPackStart hbox cvs Gtk.PackNatural 0
  Gtk.boxPackStart hbox btn Gtk.PackGrow 0
  Gtk.boxPackStart vbox hbox Gtk.PackNatural 0

fileShowRevisions :: MainCoroutine ()
fileShowRevisions = do
  rhdl <- gets (getHoodle . view (unitHoodles . currentUnit))
  let hdl = rHoodle2Hoodle rhdl
  let revs = view grevisions rhdl
  showRevisionDialog hdl revs

fileShowUUID :: MainCoroutine ()
fileShowUUID = do
  hdl <- gets (getHoodle . view (unitHoodles . currentUnit))
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
          callRenderer (GotRItem <$> cnstrctRItem itm)
          RenderEv (GotRItem ritm) <-
            waitSomeEvent (\case RenderEv (GotRItem _) -> True; _ -> False)
          --
          return (Just ritm)
    else return Nothing
