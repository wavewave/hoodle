{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.File 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
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
import           Control.Lens (view,set,over,(%~))
import           Control.Monad.State hiding (mapM,mapM_,forM_)
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Maybe (MaybeT(..))
-- import           Data.Attoparsec (parseOnly)
import           Data.ByteString (readFile)
import           Data.ByteString.Char8 as B (pack,unpack)
import qualified Data.ByteString.Lazy as L
import           Data.Digest.Pure.MD5 (md5)
import           Data.Foldable (mapM_,forM_)
import qualified Data.List as List 
import           Data.Maybe
import qualified Data.IntMap as IM
import           Data.Time.Clock
import           Filesystem.Path.CurrentOS (decodeString, encodeString)
import           Graphics.Rendering.Cairo
import           Graphics.UI.Gtk hiding (get,set)
import           System.Directory
import           System.FilePath
import qualified System.FSNotify as FS
import           System.IO (hClose, hFileSize, openFile, IOMode(..))
import           System.Process 
-- from hoodle-platform
import           Control.Monad.Trans.Crtn
import           Control.Monad.Trans.Crtn.Queue 
import           Data.Hoodle.BBox
import           Data.Hoodle.Generic
import           Data.Hoodle.Simple
import           Data.Hoodle.Select
import           Graphics.Hoodle.Render.Generic
import           Graphics.Hoodle.Render.Item
import           Graphics.Hoodle.Render.Type
import           Graphics.Hoodle.Render.Type.HitTest 
import           Text.Hoodle.Builder 
-- import qualified Text.Hoodlet.Parse.Attoparsec as Hoodlet
-- from this package 
import           Hoodle.Accessor
import           Hoodle.Coroutine.Dialog
import           Hoodle.Coroutine.Draw
import           Hoodle.Coroutine.Commit
import           Hoodle.Coroutine.Minibuffer
import           Hoodle.Coroutine.Mode 
import           Hoodle.Coroutine.Scroll
import           Hoodle.Coroutine.TextInput
import           Hoodle.ModelAction.File
import           Hoodle.ModelAction.Layer 
import           Hoodle.ModelAction.Page
import           Hoodle.ModelAction.Select
import           Hoodle.ModelAction.Select.Transform
import           Hoodle.ModelAction.Window
import qualified Hoodle.Script.Coroutine as S
import           Hoodle.Script.Hook
import           Hoodle.Type.Canvas
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Event hiding (TypSVG)
import           Hoodle.Type.HoodleState
import           Hoodle.Type.PageArrangement
import           Hoodle.Util
--
import Prelude hiding (readFile,concat,mapM,mapM_)

-- | 
askIfSave :: MainCoroutine () -> MainCoroutine () 
askIfSave action = do 
    xstate <- get 
    if not (view isSaved xstate)
      then do  
        b <- okCancelMessageBox "Current canvas is not saved yet. Will you proceed without save?" 
        case b of 
          True -> action 
          False -> return () 
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

-- | 
fileNew :: MainCoroutine () 
fileNew = do  
    xstate <- get
    xstate' <- liftIO $ getFileContent Nothing xstate 
    ncvsinfo <- liftIO $ setPage xstate' 0 (getCurrentCanvasId xstate')
    xstate'' <- return $ over currentCanvasInfo (const ncvsinfo) xstate'
    liftIO $ setTitleFromFileName xstate''
    commit xstate'' 
    invalidateAll 

-- | 
fileSave :: MainCoroutine ()
fileSave = do 
    xstate <- get 
    case view (hoodleFileControl.hoodleFileName) xstate of
      Nothing -> fileSaveAs 
      Just filename -> do     
        -- this is rather temporary not to make mistake 
        if takeExtension filename == ".hdl" 
          then do 
             put =<< (liftIO (saveHoodle xstate))
             (S.afterSaveHook filename . rHoodle2Hoodle . getHoodle) xstate
          else fileExtensionInvalid (".hdl","save") >> fileSaveAs 

-- | interleaving a monadic action between each pair of subsequent actions
sequence1_ :: (Monad m) => m () -> [m ()] -> m () 
sequence1_ _ []  = return () 
sequence1_ _ [a] = a 
sequence1_ i (a:as) = a >> i >> sequence1_ i as 

-- | 
renderjob :: RHoodle -> FilePath -> IO () 
renderjob h ofp = do 
  let p = maybe (error "renderjob") id $ IM.lookup 0 (view gpages h)  
  let Dim width height = view gdimension p  
  let rf x = cairoRenderOption (RBkgDrawPDF,DrawFull) x >> return () 
  withPDFSurface ofp width height $ \s -> renderWith s $  
    (sequence1_ showPage . map rf . IM.elems . view gpages ) h 

-- | 
fileExport :: MainCoroutine ()
fileExport = fileChooser FileChooserActionSave Nothing >>= maybe (return ()) action 
  where 
    action filename = 
      -- this is rather temporary not to make mistake 
      if takeExtension filename /= ".pdf" 
      then fileExtensionInvalid (".pdf","export") >> fileExport 
      else do      
        xstate <- get 
        let hdl = getHoodle xstate 
        liftIO (renderjob hdl filename) 

-- | 
fileStartSync :: MainCoroutine ()
fileStartSync = do 
  xst <- get 
  let mf = (,) <$> view (hoodleFileControl.hoodleFileName) xst <*> view (hoodleFileControl.lastSavedTime) xst 
  maybe (return ()) (\(filename,lasttime) -> action filename lasttime) mf  
  where  
    action filename _lasttime  = do 
      let ioact = mkIOaction $ \evhandler ->do 
            forkIO $ do 
              FS.withManager $ \wm -> do 
                origfile <- canonicalizePath filename 
                let (filedir,_) = splitFileName origfile
                print filedir 
                FS.watchDir wm (decodeString filedir) (const True) $ \ev -> do 
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
      modify (tempQueue %~ enqueue ioact) 

-- | need to be merged with ContextMenuEventSVG
exportCurrentPageAsSVG :: MainCoroutine ()
exportCurrentPageAsSVG = fileChooser FileChooserActionSave Nothing >>= maybe (return ()) action 
  where 
    action filename = 
      -- this is rather temporary not to make mistake 
      if takeExtension filename /= ".svg" 
      then fileExtensionInvalid (".svg","export") >> exportCurrentPageAsSVG 
      else do      
        cpg <- getCurrentPageCurr
        let Dim w h = view gdimension cpg 
        liftIO $ withSVGSurface filename w h $ \s -> renderWith s $ 
         cairoRenderOption (InBBoxOption Nothing) (InBBox cpg) >> return ()

-- | 
fileLoad :: FilePath -> MainCoroutine () 
fileLoad filename = do
    xstate <- get 
    xstate' <- liftIO $ getFileContent (Just filename) xstate
    ncvsinfo <- liftIO $ setPage xstate' 0 (getCurrentCanvasId xstate')
    xstateNew <- return $ over currentCanvasInfo (const ncvsinfo) xstate'
    put . set isSaved True $ xstateNew 
    let ui = view gtkUIManager xstate
    liftIO $ toggleSave ui False
    liftIO $ setTitleFromFileName xstateNew  
    clearUndoHistory 
    modeChange ToViewAppendMode 
    resetHoodleBuffers 
    invalidateAll 
    applyActionToAllCVS adjustScrollbarWithGeometryCvsId

-- | 
resetHoodleBuffers :: MainCoroutine () 
resetHoodleBuffers = do 
    liftIO $ putStrLn "resetHoodleBuffers called"
    xst <- get 
    nhdlst <- liftIO $ resetHoodleModeStateBuffers (view hoodleModeState xst)
    let nxst = set hoodleModeState nhdlst xst
    put nxst     

-- | main coroutine for open a file 
fileOpen :: MainCoroutine ()
fileOpen = do 
    mfilename <- fileChooser FileChooserActionOpen Nothing
    forM_ mfilename fileLoad 

-- | main coroutine for save as 
fileSaveAs :: MainCoroutine () 
fileSaveAs = do 
    xstate <- get 
    let hdl = (rHoodle2Hoodle . getHoodle) xstate
    maybe (defSaveAsAction xstate hdl) (\f -> liftIO (f hdl))
          (hookSaveAsAction xstate) 
  where 
    hookSaveAsAction xstate = do 
      hset <- view hookSet xstate
      saveAsHook hset
    defSaveAsAction xstate hdl = do 
        let msuggestedact = view hookSet xstate >>= fileNameSuggestionHook 
        (msuggested :: Maybe String) <- maybe (return Nothing) (liftM Just . liftIO) msuggestedact 
        mr <- fileChooser FileChooserActionSave msuggested 
        maybe (return ()) (action xstate hdl) mr 
      where action xst' hd filename = 
              if takeExtension filename /= ".hdl" 
              then fileExtensionInvalid (".hdl","save")
              else do 
                askIfOverwrite filename $ do 
                  let ntitle = B.pack . snd . splitFileName $ filename 
                      (hdlmodst',hdl') = case view hoodleModeState xst' of
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
                      xstateNew = set (hoodleFileControl.hoodleFileName) (Just filename) 
                                . set hoodleModeState hdlmodst' $ xst'
                  liftIO . L.writeFile filename . builder $ hdl'
                  put . set isSaved True $ xstateNew    
                  let ui = view gtkUIManager xstateNew
                  liftIO $ toggleSave ui False
                  liftIO $ setTitleFromFileName xstateNew 
                  S.afterSaveHook filename hdl'
          

-- | main coroutine for open a file 
fileReload :: MainCoroutine ()
fileReload = do 
    xstate <- get
    case view (hoodleFileControl.hoodleFileName) xstate of 
      Nothing -> return () 
      Just filename -> do
        if not (view isSaved xstate) 
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
    fileChooser FileChooserActionOpen Nothing >>= maybe (return ()) action 
  where 
    warning = do 
      okMessageBox "cannot load the pdf file. Check your hoodle compiled with poppler library" 
      invalidateAll 
    action filename = do  
      xstate <- get 
      let doesembed = view (settings.doesEmbedPDF) xstate
      mhdl <- liftIO $ makeNewHoodleWithPDF doesembed filename 
      flip (maybe warning) mhdl $ \hdl -> do 
        xstateNew <- return . set (hoodleFileControl.hoodleFileName) Nothing 
                     =<< (liftIO $ constructNewHoodleStateFromHoodle hdl xstate)
        commit xstateNew 
        liftIO $ setTitleFromFileName xstateNew             
        invalidateAll  
      

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
    fileChooser FileChooserActionOpen Nothing >>= maybe (return ()) embedImage

embedImage :: FilePath -> MainCoroutine ()
embedImage filename = do  
    xst <- get 
    nitm <- 
      if view (settings.doesEmbedImage) xst
        then do  
          mf <- checkEmbedImageSize filename 
          case mf of 
            Nothing -> liftIO (cnstrctRItem =<< makeNewItemImage True filename)
            Just f -> liftIO (cnstrctRItem =<< makeNewItemImage True f) 
        else
          liftIO (cnstrctRItem =<< makeNewItemImage False filename)
    let cpn = view (currentCanvasInfo . unboxLens currentPageNum) xst
    my <- autoPosText 
    let mpos = (\y->(PageNum cpn,PageCoord (50,y)))<$>my  
    insertItemAt mpos nitm 

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
    thdl <- case view hoodleModeState nxst of
      SelectState thdl' -> return thdl'
      _ -> (lift . EitherT . return . Left . Other) "insertItemAt"
    nthdl <- liftIO $ updateTempHoodleSelectIO thdl ntpg pgnum 
    put ( ( set hoodleModeState (SelectState nthdl) 
          . set isOneTimeSelectMode YesAfterSelect) nxst)
    invalidateAll  
                    
-- | 
fileLoadSVG :: MainCoroutine ()
fileLoadSVG = do 
    fileChooser FileChooserActionOpen Nothing >>= maybe (return ()) action 
  where 
    action filename = do 
      xstate <- get 
      liftIO $ putStrLn filename 
      bstr <- liftIO $ readFile filename 
      let pgnum = view (currentCanvasInfo . unboxLens currentPageNum) xstate
          hdl = getHoodle xstate 
          currpage = getPageFromGHoodleMap pgnum hdl
          currlayer = getCurrentLayer currpage
      newitem <- (liftIO . cnstrctRItem . ItemSVG) 
                   (SVG Nothing Nothing bstr (100,100) (Dim 300 300))
      let otheritems = view gitems currlayer  
      let ntpg = makePageSelectMode currpage (otheritems :- (Hitted [newitem]) :- Empty)  
      modeChange ToSelectMode 
      nxstate <- get 
      thdl <- case view hoodleModeState nxstate of
                SelectState thdl' -> return thdl'
                _ -> (lift . EitherT . return . Left . Other) "fileLoadSVG"
      nthdl <- liftIO $ updateTempHoodleSelectIO thdl ntpg pgnum 
      put ( ( set hoodleModeState (SelectState nthdl) 
            . set isOneTimeSelectMode YesAfterSelect) nxstate)
      invalidateAll 

-- |
askQuitProgram :: MainCoroutine () 
askQuitProgram = do 
    b <- okCancelMessageBox "Current canvas is not saved yet. Will you close hoodle?" 
    case b of 
      True -> liftIO mainQuit
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
  xst <- get 
  let hdl = getHoodle xst
  nhdl <- liftIO . embedPDFInHoodle $ hdl
  modeChange ToViewAppendMode
  commit (set hoodleModeState (ViewAppendState nhdl) xst)
  invalidateAll   
  
-- | embed an item from hoodlet using hoodlet identifier
embedHoodlet :: String -> MainCoroutine ()
embedHoodlet str = liftIO (loadHoodlet str) >>= mapM_ (insertItemAt Nothing) 

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


mkRevisionPdfFile :: RHoodle -> String -> IO ()
mkRevisionPdfFile hdl fname = do 
    hdir <- getHomeDirectory
    tempfile <- mkTmpFile "pdf"
    renderjob hdl tempfile 
    let nfilename = fname <.> "pdf"
        vcsdir = hdir </> ".hoodle.d" </> "vcs"
    b <- doesDirectoryExist vcsdir 
    unless b $ createDirectory vcsdir
    renameFile tempfile (vcsdir </> nfilename)  

-- | 
fileVersionSave :: MainCoroutine () 
fileVersionSave = do 
    rhdl <- getHoodle <$> get 
    let hdl = rHoodle2Hoodle rhdl 
    rmini <- minibufDialog "Commit Message:"
    case rmini of 
      Right [] -> return ()
      Right strks' -> do
        doIOaction $ \_evhandler -> do 
          (md5str,fname) <- mkRevisionHdlFile hdl
          mkRevisionPdfFile rhdl fname
          return (UsrEv (GotRevisionInk md5str strks'))
        r <- waitSomeEvent (\case GotRevisionInk _ _ -> True ; _ -> False )
        let GotRevisionInk md5str strks = r          
            nrev = RevisionInk (B.pack md5str) strks
        modify (\xst -> let hdlmodst = view hoodleModeState xst 
                        in case hdlmodst of 
                             ViewAppendState rhdl' -> 
                               let nrhdl = over grevisions (<> [nrev]) rhdl' 
                               in set hoodleModeState (ViewAppendState nrhdl) xst 
                             SelectState thdl -> 
                               let nthdl = over gselRevisions (<> [nrev]) thdl
                               in set hoodleModeState (SelectState nthdl) xst)
        commit_ 
      Left () -> do 
        txtstr <- maybe "" id <$> textInputDialog
        doIOaction $ \_evhandler -> do 
          (md5str,fname) <- mkRevisionHdlFile hdl
          mkRevisionPdfFile rhdl fname
          return (UsrEv (GotRevision md5str txtstr))
        r <- waitSomeEvent (\case GotRevision _ _ -> True ; _ -> False )
        let GotRevision md5str txtstr' = r          
            nrev = Revision (B.pack md5str) (B.pack txtstr')
        modify (\xst -> let hdlmodst = view hoodleModeState xst 
                        in case hdlmodst of 
                             ViewAppendState rhdl' -> 
                               let nrhdl = over grevisions (<> [nrev]) rhdl' 
                               in set hoodleModeState (ViewAppendState nrhdl) xst 
                             SelectState thdl -> 
                               let nthdl = over gselRevisions (<> [nrev]) thdl
                               in set hoodleModeState (SelectState nthdl) xst)
        commit_ 



showRevisionDialog :: Hoodle -> [Revision] -> MainCoroutine ()
showRevisionDialog hdl revs = 
    modify (tempQueue %~ enqueue action) 
    >> waitSomeEvent (\case GotOk -> True ; _ -> False)
    >> return ()
  where 
    action = mkIOaction $ \_evhandler -> do 
               dialog <- dialogNew
               vbox <- dialogGetUpper dialog
               mapM_ (addOneRevisionBox vbox hdl) revs 
               _btnOk <- dialogAddButton dialog "Ok" ResponseOk
               widgetShowAll dialog
               _res <- dialogRun dialog
               widgetDestroy dialog
               return (UsrEv GotOk)


mkPangoText :: String -> Render ()
mkPangoText str = do 
    let pangordr = do 
          ctxt <- cairoCreateContext Nothing 
          layout <- layoutEmpty ctxt   
          fdesc <- fontDescriptionNew 
          fontDescriptionSetFamily fdesc "Sans Mono"
          fontDescriptionSetSize fdesc 8.0 
          layoutSetFontDescription layout (Just fdesc)
          layoutSetWidth layout (Just 250)
          layoutSetWrap layout WrapAnywhere 
          layoutSetText layout str 
          -- (_,reclog) <- layoutGetExtents layout 
          -- let PangoRectangle x y w h = reclog 
          return layout
        rdr layout = do setSourceRGBA 0 0 0 1
                        updateLayout layout 
                        showLayout layout 
    layout <- liftIO $  pangordr 
    rdr layout

addOneRevisionBox :: VBox -> Hoodle -> Revision -> IO ()
addOneRevisionBox vbox hdl rev = do 
    cvs <- drawingAreaNew 
    cvs `on` sizeRequest $ return (Requisition 250 25)
    cvs `on` exposeEvent $ tryEvent $ do 
      drawwdw <- liftIO $ widgetGetDrawWindow cvs 
      liftIO . renderWithDrawable drawwdw $ do 
        case rev of 
          RevisionInk _ strks -> scale 0.5 0.5 >> mapM_ cairoRender strks
          Revision _ txt -> mkPangoText (B.unpack txt)            
    hdir <- getHomeDirectory
    let vcsdir = hdir </> ".hoodle.d" </> "vcs"
    btn <- buttonNewWithLabel "view"
    btn `on` buttonPressEvent $ tryEvent $ do 
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
    hbox <- hBoxNew False 0
    boxPackStart hbox cvs PackNatural 0
    boxPackStart hbox btn PackGrow  0
    boxPackStart vbox hbox PackNatural 0

fileShowRevisions :: MainCoroutine ()
fileShowRevisions = do 
    rhdl <- liftM getHoodle get  
    let hdl = rHoodle2Hoodle rhdl
    let revs = view grevisions rhdl
    showRevisionDialog hdl revs 
  
fileShowUUID :: MainCoroutine ()
fileShowUUID = do 
    hdl <- liftM getHoodle get  
    let uuidstr = view ghoodleID hdl
    okMessageBox (B.unpack uuidstr)
  

  
  
  