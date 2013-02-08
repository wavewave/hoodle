{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import           Control.Category
import           Control.Lens
import           Control.Monad.State hiding (mapM)
import           Control.Monad.Trans.Either
import           Data.ByteString (readFile,concat)
import qualified Data.ByteString as S (writeFile,hGet)
import           Data.ByteString.Base64 
import           Data.ByteString.Char8 as B (pack,unpack,ByteString(..))
import qualified Data.ByteString.Lazy as L
--- import           Data.List (intercale ) 
import           Data.Maybe
import           Data.Traversable (mapM)
import qualified Data.IntMap as IM
import           Graphics.GD.ByteString 
import           Graphics.Rendering.Cairo
import           Graphics.UI.Gtk hiding (get,set)
#ifdef POPPLER
import qualified Graphics.UI.Gtk.Poppler.Document as Poppler
import qualified Graphics.UI.Gtk.Poppler.Page as PopplerPage
#endif
import           System.Directory
import           System.Exit 
import           System.FilePath
import           System.Process 
-- from hoodle-platform
import           Control.Monad.Trans.Crtn
import           Control.Monad.Trans.Crtn.Event
import           Control.Monad.Trans.Crtn.Queue 
import           Data.Hoodle.Generic
import           Data.Hoodle.Simple
import           Data.Hoodle.Select
import           Graphics.Hoodle.Render.Generic
import           Graphics.Hoodle.Render.Item
import           Graphics.Hoodle.Render.Type
import           Graphics.Hoodle.Render.Type.HitTest 
import           Hoodle.Util.Process
import           Text.Hoodle.Builder 
-- from this package 
import           Hoodle.Accessor
import           Hoodle.Coroutine.Draw
import           Hoodle.Coroutine.Commit
import           Hoodle.Coroutine.Mode 
import           Hoodle.Coroutine.Scroll
import           Hoodle.ModelAction.File
import           Hoodle.ModelAction.Layer 
import           Hoodle.ModelAction.Page
import           Hoodle.ModelAction.Select
import           Hoodle.ModelAction.Window
import qualified Hoodle.Script.Coroutine as S
import           Hoodle.Script.Hook
-- import           Hoodle.Type.Alias
import           Hoodle.Type.Canvas
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Event hiding (SVG)
import           Hoodle.Type.HoodleState
import           Hoodle.Util
import           Hoodle.View.Draw
--
import Prelude hiding ((.),id,readFile,concat,mapM)


-- | 
waitSomeEvent :: (MyEvent -> Bool) -> MainCoroutine MyEvent 
waitSomeEvent p = do 
    r <- nextevent
    case r of 
      UpdateCanvas cid -> invalidateInBBox Nothing Efficient cid 
                          >> waitSomeEvent p  -- this is temporary
      _ -> if  p r then return r else waitSomeEvent p  


-- |
okMessageBox :: String -> MainCoroutine () 
okMessageBox msg = modify (tempQueue %~ enqueue action) 
                   >> waitSomeEvent (==GotOk) 
                   >> return () 
  where 
    action = Left . ActionOrder $ 
               \_evhandler -> do 
                 dialog <- messageDialogNew Nothing [DialogModal]
                   MessageQuestion ButtonsOk msg 
                 _res <- dialogRun dialog 
                 widgetDestroy dialog 
                 return GotOk 

-- | 
okCancelMessageBox :: String -> MainCoroutine Bool 
okCancelMessageBox msg = modify (tempQueue %~ enqueue action) 
                         >> waitSomeEvent p >>= return . p 
  where 
    p (OkCancel b) = b -- True 
    p _ = False 
    action = Left . ActionOrder $ 
               \_evhandler -> do 
                 dialog <- messageDialogNew Nothing [DialogModal]
                   MessageQuestion ButtonsOkCancel msg 
                 res <- dialogRun dialog 
                 let b = case res of 
                           ResponseOk -> True
                           _ -> False
                 widgetDestroy dialog 
                 return (OkCancel b)

-- | 
fileChooser :: FileChooserAction -> Maybe String -> MainCoroutine (Maybe FilePath) 
fileChooser choosertyp mfname = do 
    mrecentfolder <- S.recentFolderHook 
    modify (tempQueue %~ enqueue (action mrecentfolder)) >> go 
  where 
    go = do r <- nextevent                   
            case r of 
              FileChosen b -> return b  
              UpdateCanvas cid -> invalidateInBBox Nothing Efficient cid >> go  -- this is temporary
              o -> liftIO (print o) >> go 
    action mrf = Left . ActionOrder $ \_evhandler -> do 
      dialog <- fileChooserDialogNew Nothing Nothing choosertyp 
                  [ ("OK", ResponseOk) 
                  , ("Cancel", ResponseCancel) ]
      case mrf of 
        Just rf -> fileChooserSetCurrentFolder dialog rf 
        Nothing -> getCurrentDirectory >>= fileChooserSetCurrentFolder dialog 
      maybe (return ()) (fileChooserSetCurrentName dialog) mfname 
      res <- dialogRun dialog
      mr <- case res of 
              ResponseDeleteEvent -> return Nothing
              ResponseOk ->  fileChooserGetFilename dialog 
              ResponseCancel -> return Nothing 
              _ -> putStrLn "??? in fileOpen" >> return Nothing 
      widgetDestroy dialog
      return (FileChosen mr)

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
    case view currFileName xstate of
      Nothing -> fileSaveAs 
      Just filename -> do     
        -- this is rather temporary not to make mistake 
        if takeExtension filename == ".hdl" 
          then do 
             let hdl = (rHoodle2Hoodle . getHoodle) xstate 
             liftIO . L.writeFile filename . builder $ hdl
             put . set isSaved True $ xstate 
             let ui = view gtkUIManager xstate
             liftIO $ toggleSave ui False
             S.afterSaveHook filename hdl
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
  let rf :: InBBox RPage -> Render ()
      rf x = cairoRenderOption (InBBoxOption Nothing) x >> return ()  
  withPDFSurface ofp width height $ \s -> renderWith s $  
    -- (sequence1_ showPage . map renderPage . hoodle_pages) h 
    (sequence1_ showPage . map (rf . InBBox) . IM.elems . view gpages ) h 

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
        let hdl = getHoodle xstate -- (rHoodle2Hoodle . getHoodle) xstate 
        liftIO (renderjob hdl filename) 


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
    put . set isSaved True 
      $ xstateNew 
    liftIO $ setTitleFromFileName xstateNew  
    clearUndoHistory 
    modeChange ToViewAppendMode 
    resetHoodleBuffers 
    invalidateAll 
    applyActionToAllCVS adjustScrollbarWithGeometryCvsId


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
  case mfilename of 
    Nothing -> return ()
    Just filename -> fileLoad filename 

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
                    xstateNew = set currFileName (Just filename) 
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
    case view currFileName xstate of 
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
      mhdl <- liftIO $ makeNewHoodleWithPDF filename 
      flip (maybe warning) mhdl $ \hdl -> do 
        xstateNew <- return . set currFileName Nothing 
                     =<< (liftIO $ constructNewHoodleStateFromHoodle hdl xstate)
        commit xstateNew 
        liftIO $ setTitleFromFileName xstateNew             
        invalidateAll  
      

-- | 
fileLoadPNGorJPG :: MainCoroutine ()
fileLoadPNGorJPG = do 
    fileChooser FileChooserActionOpen Nothing >>= maybe (return ()) action 
  where 
    action filename = do  
      xstate <- get 
      liftIO $ putStrLn filename 
      let pgnum = unboxGet currentPageNum . view currentCanvasInfo $ xstate
          hdl = getHoodle xstate 
          currpage = getPageFromGHoodleMap pgnum hdl
          currlayer = getCurrentLayer currpage
          isembedded = view (settings.doesEmbedImage) xstate 
      newitem <- liftIO (cnstrctRItem =<< makeNewItemImage isembedded filename) 
      
      let otheritems = view gitems currlayer  
      let ntpg = makePageSelectMode currpage (otheritems :- (Hitted [newitem]) :- Empty)  
      modeChange ToSelectMode 
      nxstate <- get 
      thdl <- case view hoodleModeState nxstate of
                SelectState thdl' -> return thdl'
                _ -> (lift . EitherT . return . Left . Other) "fileLoadPNGorJPG"
      nthdl <- liftIO $ updateTempHoodleSelectIO thdl ntpg pgnum 
      let nxstate2 = set hoodleModeState (SelectState nthdl) nxstate
      put nxstate2
      invalidateAll 



-- | 
makeNewItemImage :: Bool  -- ^ isEmbedded?
                    -> FilePath 
                    -> IO Item
makeNewItemImage isembedded filename = 
    if isembedded 
      then let fileext = takeExtension filename 
               imgaction 
                 | fileext == ".PNG" || fileext == ".png" = loadpng 
                 | fileext == ".JPG" || fileext == ".jpg" = loadjpg 
                 | otherwise = loadsrc 
           in imgaction 
      else loadsrc 
  where loadsrc = (return . ItemImage) (Image (B.pack filename) (100,100) (Dim 300 300))
        loadpng = do 
          img <- loadPngFile filename
          (w,h) <- imageSize img 
          let dim | w >= h = Dim 300 (fromIntegral h*300/fromIntegral w)
                  | otherwise = Dim (fromIntegral w*300/fromIntegral h) 300 
          bstr <- savePngByteString img 
          let b64str = encode bstr 
              ebdsrc = "data:image/png;base64," <> b64str
          return . ItemImage $ Image ebdsrc (100,100) dim -- (Dim 300 300)
        loadjpg = do 
          img <- loadJpegFile filename
          (w,h) <- imageSize img 
          let dim | w >= h = Dim 300 (fromIntegral h*300/fromIntegral w)
                  | otherwise = Dim (fromIntegral w*300/fromIntegral h) 300 
          bstr <- savePngByteString img 
          let b64str = encode bstr 
              ebdsrc = "data:image/png;base64," <> b64str
          return . ItemImage $ Image ebdsrc (100,100) dim -- (Dim 300 300)   
            
-- | 
fileLoadSVG :: MainCoroutine ()
fileLoadSVG = do 
    fileChooser FileChooserActionOpen Nothing >>= maybe (return ()) action 
  where 
    action filename = do 
      xstate <- get 
      liftIO $ putStrLn filename 
      bstr <- liftIO $ readFile filename 
      let pgnum = unboxGet currentPageNum . view currentCanvasInfo $ xstate
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
      let nxstate2 = set hoodleModeState (SelectState nthdl) nxstate
      put nxstate2
      invalidateAll 


fileLaTeX :: MainCoroutine () 
fileLaTeX = do modify (tempQueue %~ enqueue action) 
               minput <- go
               case minput of 
                 Nothing -> return () 
                 Just (latex,svg) -> afteraction (latex,svg) 
  where 
    go = do r <- nextevent
            case r of 
              LaTeXInput input -> return input 
              _ -> go 
    action = Left . ActionOrder $ 
               \_evhandler -> do 
                 dialog <- messageDialogNew Nothing [DialogModal]
                   MessageQuestion ButtonsOkCancel "latex input"
                 vbox <- dialogGetUpper dialog
                 -- entry <- entryNew 
                 -- boxPackStart vbox entry PackGrow 0
                 txtvw <- textViewNew
                 boxPackStart vbox txtvw PackGrow 0 
                 widgetShowAll dialog
                 res <- dialogRun dialog 
                 case res of 
                   ResponseOk -> do 
                     -- l <- entryGetText entry
                     buf <- textViewGetBuffer txtvw 
                     istart <- textBufferGetStartIter buf
                     iend <- textBufferGetEndIter buf
                     l <- textBufferGetText buf istart iend True
                     widgetDestroy dialog
                     tdir <- getTemporaryDirectory
                     writeFile (tdir </> "latextest.tex") l 
                     let cmd = "lasem-render-0.6 " ++ (tdir </> "latextest.tex") ++ " -f svg -o " ++ (tdir </> "latextest.svg" )
                     print cmd 
                     excode <- system cmd 
                     case excode of 
                       ExitSuccess -> do 
                         svg <- readFile (tdir </> "latextest.svg")
                         return (LaTeXInput (Just (B.pack l,svg)))
                       _ -> return (LaTeXInput Nothing)

                   _ -> do 
                     widgetDestroy dialog
                     return (LaTeXInput Nothing)
    afteraction (latex,svg) = do 
      xstate <- get 
      let pgnum = unboxGet currentPageNum . view currentCanvasInfo $ xstate
          hdl = getHoodle xstate 
          currpage = getPageFromGHoodleMap pgnum hdl
          currlayer = getCurrentLayer currpage
      newitem <- (liftIO . cnstrctRItem . ItemSVG) 
          (SVG (Just latex) Nothing svg (100,100) (Dim 300 50))
      let otheritems = view gitems currlayer  
      let ntpg = makePageSelectMode currpage (otheritems :- (Hitted [newitem]) :- Empty)  
      modeChange ToSelectMode 
      nxstate <- get 
      thdl <- case view hoodleModeState nxstate of
                SelectState thdl' -> return thdl'
                _ -> (lift . EitherT . return . Left . Other) "fileLaTeX"
      nthdl <- liftIO $ updateTempHoodleSelectIO thdl ntpg pgnum 
      let nxstate2 = set hoodleModeState (SelectState nthdl) nxstate
      put nxstate2
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
    liftIO $ putStrLn "embedPredefinedImage"
    mpredefined <- S.embedPredefinedImageHook 
    liftIO $ print mpredefined
    case mpredefined of 
      Nothing -> return () 
      Just filename -> do 
        xstate <- get 
        let pgnum = unboxGet currentPageNum . view currentCanvasInfo $ xstate
            hdl = getHoodle xstate 
            currpage = getPageFromGHoodleMap pgnum hdl
            currlayer = getCurrentLayer currpage
            isembedded = True
        newitem <- liftIO (cnstrctRItem =<< makeNewItemImage isembedded filename) 

        let otheritems = view gitems currlayer  
        let ntpg = makePageSelectMode currpage (otheritems :- (Hitted [newitem]) :- Empty)  
        modeChange ToSelectMode 
        nxstate <- get 
        thdl <- case view hoodleModeState nxstate of
                  SelectState thdl' -> return thdl'
                  _ -> (lift . EitherT . return . Left . Other) "embedPredefinedImage"
        nthdl <- liftIO $ updateTempHoodleSelectIO thdl ntpg pgnum 
        let nxstate2 = set isOneTimeSelectMode YesAfterSelect 
                     . set hoodleModeState (SelectState nthdl) 
                     $ nxstate
        put nxstate2
        invalidateAll 
        
-- | this is temporary. I will remove it
embedPredefinedImage2 :: MainCoroutine () 
embedPredefinedImage2 = do 
    liftIO $ putStrLn "embedPredefinedImage2"
    mpredefined <- S.embedPredefinedImage2Hook 
    liftIO $ print mpredefined
    case mpredefined of 
      Nothing -> return () 
      Just filename -> do 
        xstate <- get 
        let pgnum = unboxGet currentPageNum . view currentCanvasInfo $ xstate
            hdl = getHoodle xstate 
            currpage = getPageFromGHoodleMap pgnum hdl
            currlayer = getCurrentLayer currpage
            isembedded = True
        newitem <- liftIO (cnstrctRItem =<< makeNewItemImage isembedded filename) 

        let otheritems = view gitems currlayer  
        let ntpg = makePageSelectMode currpage (otheritems :- (Hitted [newitem]) :- Empty)  
        modeChange ToSelectMode 
        nxstate <- get 
        thdl <- case view hoodleModeState nxstate of
                  SelectState thdl' -> return thdl'
                  _ -> (lift . EitherT . return . Left . Other) "embedPredefinedImage2"
        nthdl <- liftIO $ updateTempHoodleSelectIO thdl ntpg pgnum 
        let nxstate2 = set isOneTimeSelectMode YesAfterSelect 
                     . set hoodleModeState (SelectState nthdl) 
                     $ nxstate
        put nxstate2
        invalidateAll         
        
-- | this is temporary. I will remove it
embedPredefinedImage3 :: MainCoroutine () 
embedPredefinedImage3 = do 
    liftIO $ putStrLn "embedPredefinedImage3"
    mpredefined <- S.embedPredefinedImage3Hook 
    liftIO $ print mpredefined
    case mpredefined of 
      Nothing -> return () 
      Just filename -> do 
        xstate <- get 
        let pgnum = unboxGet currentPageNum . view currentCanvasInfo $ xstate
            hdl = getHoodle xstate 
            currpage = getPageFromGHoodleMap pgnum hdl
            currlayer = getCurrentLayer currpage
            isembedded = True
        newitem <- liftIO (cnstrctRItem =<< makeNewItemImage isembedded filename) 

        let otheritems = view gitems currlayer  
        let ntpg = makePageSelectMode currpage (otheritems :- (Hitted [newitem]) :- Empty)  
        modeChange ToSelectMode 
        nxstate <- get 
        thdl <- case view hoodleModeState nxstate of
                  SelectState thdl' -> return thdl'
                  _ -> (lift . EitherT . return . Left . Other) "embedPredefinedImage3"
        nthdl <- liftIO $ updateTempHoodleSelectIO thdl ntpg pgnum 
        let nxstate2 = set isOneTimeSelectMode YesAfterSelect 
                     . set hoodleModeState (SelectState nthdl) 
                     $ nxstate
        put nxstate2
        invalidateAll         
        
-- | this is very temporary, need to be changed.     
findFirstPDFFile :: [(Int,RPage)] -> Maybe B.ByteString
findFirstPDFFile xs = let ys = (filter isJust . map f) xs 
                      in if null ys then Nothing else head ys 
  where f (_,p) = case view gbackground p of 
                    RBkgPDF _ f _ _ _ -> Just f
                    _ -> Nothing 
      
findAllPDFPages :: [(Int,RPage)] -> [Int]
findAllPDFPages = catMaybes . map f
  where f (n,p) = case view gbackground p of 
                    RBkgPDF _ f _ _ _ -> Just n
                    _ -> Nothing 

replacePDFPages :: [(Int,RPage)] -> [(Int,RPage)] 
replacePDFPages xs = map f xs 
  where f (n,p) = case view gbackground p of 
                    RBkgPDF _ f pdfn mpdf msfc -> (n, set gbackground (RBkgEmbedPDF pdfn mpdf msfc) p)
                    _ -> (n,p) 
        
-- | 
embedPDFInHoodle :: RHoodle -> IO RHoodle
embedPDFInHoodle hdl = do 
    let pgs = (IM.toAscList . view gpages) hdl  
        mfn = findFirstPDFFile pgs
        allpdfpg = findAllPDFPages pgs 
        
    case mfn of 
      Nothing -> return hdl 
      Just fn -> do 
        let fnstr = B.unpack fn 
            pglst = map show allpdfpg 
            cmdargs =  [fnstr, "cat"] ++ pglst ++ ["output", "-"]
        print cmdargs 
        (_,Just hout,_,_) <- createProcess (proc "pdftk" cmdargs) { std_out = CreatePipe } 
        bstr <- L.hGetContents hout
        let b64str = (encode . concat . L.toChunks) bstr 
            ebdsrc = Just ("data:application/x-pdf;base64," <> b64str)
            npgs = (IM.fromAscList . replacePDFPages) pgs 
        (return . set gembeddedpdf ebdsrc . set gpages npgs) hdl
{-        
        
    let Dim w h = view gdimension pg 
        bkg = view gbackground pg   
    case bkg of 
      RBkgPDF _ f n mpdf msfc -> do 
        case mpdf of 
          Nothing -> return pg 
          Just pdf -> do 
            let args = ["A="++B.unpack f, "cat", "A"++ show n, "output", "-"] 
            print args 
            (_,Just hout,_,_) <- createProcess (proc "pdftk" args) { std_out = CreatePipe } 
            

            bstr <- L.hGetContents hout

            let -- bstr = B.pack str 
                b64str = (encode . concat . L.toChunks) bstr 
                ebdsrc = "data:application/x-pdf;base64," <> b64str
            let nbkg = RBkgEmbedPDF n mpdf msfc 
            return (set gbackground nbkg pg)
      _ -> return pg 
-}

-- | 
embedAllPDFBackground :: MainCoroutine () 
embedAllPDFBackground = do 
  xst <- get 
  let hdl = getHoodle xst
  nhdl <- liftIO . embedPDFInHoodle $ hdl
  modeChange ToViewAppendMode
  commit (set hoodleModeState (ViewAppendState nhdl) xst)
  invalidateAll   