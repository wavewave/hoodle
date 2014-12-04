{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Window 
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Window where

import           Control.Applicative
import           Control.Lens (view,set,over,(^.),(.~),_2)
import           Control.Monad.State 
import qualified Data.IntMap as M
import qualified Data.List as L
import           Data.UUID (UUID)
import qualified Graphics.UI.Gtk as Gtk
--
import           Data.Hoodle.Generic
import           Data.Hoodle.Simple (Dimension(..))
--
import           Hoodle.Accessor
import           Hoodle.Coroutine.Draw
import           Hoodle.Coroutine.File
import           Hoodle.Coroutine.Mode
import           Hoodle.Coroutine.Page
import           Hoodle.GUI.Reflect
import           Hoodle.ModelAction.Page
import           Hoodle.ModelAction.Window
import           Hoodle.Type.Canvas
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Enum
import           Hoodle.Type.Event
import           Hoodle.Type.HoodleState
import           Hoodle.Type.PageArrangement
import           Hoodle.Type.Undo
import           Hoodle.Type.Window
import           Hoodle.Util
--

-- | canvas configure with general zoom update func
canvasConfigureGenUpdate :: MainCoroutine () 
                            -> CanvasId 
                            -> CanvasDimension 
                            -> MainCoroutine () 
canvasConfigureGenUpdate updatefunc cid cdim 
  = updateUhdl (\uhdl -> unboxBiAct (fsingle uhdl) (fcont uhdl) (getCanvasInfo cid uhdl)) >> updatefunc 
  where fsingle uhdl cinfo = do 
          cinfo' <- liftIO $ updateCanvasDimForSingle cdim cinfo 
          return $ setCanvasInfo (cid,CanvasSinglePage cinfo') uhdl
        fcont uhdl cinfo = do 
          page <- getCurrentPageCvsId cid
          let pdim = PageDimension (view gdimension page)
          cinfo' <- liftIO $ updateCanvasDimForContSingle pdim cdim cinfo 
          return $ setCanvasInfo (cid,CanvasContPage cinfo') uhdl 
  
-- | 
doCanvasConfigure :: CanvasId -> CanvasDimension -> MainCoroutine () 
doCanvasConfigure = canvasConfigureGenUpdate canvasZoomUpdateAll

-- | 
eitherSplit :: SplitType -> MainCoroutine () 
eitherSplit stype = do
    xst <- get
    let uhdl = view (unitHoodles.currentUnit) xst
    let cmap = view cvsInfoMap uhdl
        currcid = getCurrentCanvasId uhdl
        newcid = newCanvasId cmap 
        fstate = view frameState uhdl
        enewfstate = splitWindow currcid (newcid,stype) fstate 
    case enewfstate of 
      Left _ -> return ()
      Right fstate' -> do 
        cinfobox <- maybeError "eitherSplit" . M.lookup currcid $ cmap 
        let callback = view callBack xst
            rtwin = view rootWindow uhdl
            rtcntr = view rootContainer uhdl
            rtrwin = view rootOfRootWindow xst 
        liftIO $ Gtk.containerRemove rtcntr rtwin
        (uhdl',win,fstate'') <- liftIO $ constructFrame' callback cinfobox uhdl fstate'
        let uhdl'' = ((frameState .~ fstate'') . (rootWindow .~ win)) uhdl'
        let xst3 = (unitHoodles.currentUnit .~ uhdl'') xst
        put xst3 
        liftIO $ registerFrameToContainer rtrwin rtcntr win
        (uhdl3,_wconf) <- liftIO $ eventConnect xst3 uhdl'' (view frameState uhdl'')
        updateUhdl $ const (liftIO $ updatePageAll (view hoodleModeState uhdl3) uhdl3)
        canvasZoomUpdateAll
        invalidateAll 

-- | 
deleteCanvas :: MainCoroutine () 
deleteCanvas = do 
    xst <- get
    let uhdl = view (unitHoodles.currentUnit) xst
        cmap = view cvsInfoMap uhdl
        currcid = getCurrentCanvasId uhdl
        fstate = view frameState uhdl
        enewfstate = removeWindow currcid fstate 
    case enewfstate of 
      Left _ -> return ()
      Right Nothing -> return ()
      Right (Just fstate') -> do 
        let cmap' = M.delete currcid cmap
            newcurrcid = maximum (M.keys cmap')
        updateUhdl $ \_uhdl -> do
          uhdl' <- changeCurrentCanvasId newcurrcid 
          maybe (return uhdl') return $ setCanvasInfoMap cmap' uhdl'
        xst1 <- get
        let uhdl1 = view (unitHoodles.currentUnit) xst1
        let rtwin = view rootWindow uhdl1
            rtcntr = view rootContainer uhdl1 
            rtrwin = view rootOfRootWindow xst1 
        liftIO $ Gtk.containerRemove rtcntr rtwin
        (uhdl2,win,fstate'') <- liftIO $ constructFrame xst1 uhdl1 fstate'
        pureUpdateUhdl (const (((frameState .~ fstate'') . (rootWindow .~ win)) uhdl2))
        xst3 <- get
        liftIO $ registerFrameToContainer rtrwin rtcntr win
        updateUhdl $ \uhdl' -> do
          (uhdl'',_wconf) <- liftIO $ eventConnect xst3 uhdl' (view frameState uhdl')
          liftIO $ updatePageAll (view hoodleModeState uhdl'') uhdl''
        canvasZoomUpdateAll
        invalidateAll 

            
-- | 
paneMoveStart :: MainCoroutine () 
paneMoveStart = do 
    ev <- nextevent 
    case ev of 
      UpdateCanvas cid -> invalidateInBBox Nothing Efficient cid >> paneMoveStart 
      PaneMoveEnd -> return () 
      CanvasConfigure cid w' h'->
        canvasConfigureGenUpdate canvasZoomUpdateBufAll cid (CanvasDimension (Dim w' h')) >> paneMoveStart
      _ -> paneMoveStart
       
-- | not yet implemented?
paneMoved :: MainCoroutine () 
paneMoved = do 
    liftIO $ putStrLn "pane moved called"
    
-- | start full screen mode
fullScreen :: MainCoroutine ()
fullScreen = do 
    xst <- get 
    let rwin = view rootOfRootWindow xst 
    if view isFullScreen xst 
      then liftIO (Gtk.windowUnfullscreen rwin) >> modify (over isFullScreen (const False))
      else liftIO (Gtk.windowFullscreen rwin) >> modify (over isFullScreen (const True))

-- |
addTab :: Maybe FilePath -> MainCoroutine ()
addTab mfp = do
    xst <- get
    let notebook = xst ^. rootNotebook
        callback = xst ^. callBack
        btnold = xst ^. (unitHoodles.currentUnit.unitButton)
    liftIO $ Gtk.widgetSetSensitive btnold False
    vboxcvs <- liftIO $ Gtk.vBoxNew False 0
    (tabnum,uuid,btn) <- liftIO $ createTab callback notebook vboxcvs
    let wconf = Node 1
        initcvs = defaultCvsInfoSinglePage { _canvasId = 1 }
        initcvsbox = CanvasSinglePage initcvs
    uhdl' <- (undoTable .~ emptyUndo 50)   -- (undo = 50 for the time being) 
             . (frameState .~ wconf) 
             . updateFromCanvasInfoAsCurrentCanvas initcvsbox
             . (cvsInfoMap .~ M.empty)
             . (isSaved .~ maybe False (const True) mfp)
             . (hoodleFileControl.hoodleFileName .~ mfp) 
             . (unitKey .~ tabnum) 
             . (unitUUID .~ uuid) 
             . (unitButton .~ btn)
             <$> liftIO emptyUnitHoodle
    modify . set (unitHoodles.currentUnit) =<< (liftIO $ do
      (uhdl'',wdgt,_) <- constructFrame xst uhdl' wconf
      let uhdl3 = (rootWindow .~ wdgt) . (rootContainer .~ Gtk.castToBox vboxcvs) $ uhdl''
      (uhdl4,_wconf) <- eventConnect xst uhdl3 (view frameState uhdl3)
      registerFrameToContainer (xst^.rootOfRootWindow) (Gtk.castToBox vboxcvs) wdgt
      Gtk.widgetShowAll notebook
      return uhdl4)
    doIOaction_ $ 
      blockWhile (view (uiComponentSignalHandler.switchTabSignal) xst) $
        Gtk.set notebook [Gtk.notebookPage Gtk.:= tabnum]
    getFileContent mfp
    updateUhdl $ \uhdl -> do
      liftIO (updatePageAll (view hoodleModeState uhdl) uhdl)
      unboxBiAct (sing2ContPage uhdl) (const (return uhdl)) . view currentCanvasInfo $ uhdl
    pageZoomChange FitWidth
    invalidateAll 

-- | 
findTab :: UUID -> MainCoroutine (Maybe Int)
findTab uuid = do
    uhdlmap <- view (unitHoodles._2) <$> get
    let assoc = (map (\x -> (view unitUUID x,view unitKey x)) . M.elems) uhdlmap
    return (L.lookup uuid assoc)

-- |
switchTab :: Int -> MainCoroutine ()
switchTab tabnum = do
    xst <- get
    let notebook = view rootNotebook xst
    doIOaction_ $ Gtk.set notebook [Gtk.notebookPage Gtk.:= tabnum ]
    uhdls <- view unitHoodles <$> get

    let current = fst uhdls
    when (tabnum /= current) $ do 
      let uhdl = fromJustError "switchTab"  (M.lookup tabnum (snd uhdls))
      liftIO $ Gtk.widgetSetSensitive (uhdls^.(currentUnit.unitButton)) False
      liftIO $ Gtk.widgetSetSensitive (uhdl^.unitButton) True
      modify $ (unitHoodles.currentUnit .~ uhdl)
      updateUhdl $ \uhdl' -> liftIO (updatePageAll (view hoodleModeState uhdl') uhdl')
#ifndef GTK3
      view currentCanvasInfo uhdl # 
        forBoth' unboxBiAct $ \cinfo -> do
          (w,h) <- liftIO $ Gtk.widgetGetSize (cinfo^.drawArea) 
          doCanvasConfigure (cinfo^.canvasId) (CanvasDimension (Dim (fromIntegral w) (fromIntegral h)))
#endif // not GTK3
      invalidateAll 
      liftIO $ reflectUIToggle (xst ^. gtkUIManager) "SAVEA" (not (uhdl ^. isSaved))

-- |
closeTab :: MainCoroutine ()
closeTab = do 
  xst <- get
  let (currk,uhdlmap) = xst ^. unitHoodles
      uhdlmap' = M.delete currk uhdlmap 
      uhdllst'' = (map (\(x,y)-> (x,(unitKey .~ x) y)) . zip [0..] . M.elems) uhdlmap'
      uhdlmap'' = M.fromList uhdllst''
      sz = M.size uhdlmap
  if (sz > 1) 
    then do
      let notebook = xst ^. rootNotebook
      doIOaction_ $ Gtk.notebookRemovePage notebook currk
      modify $ (unitHoodles .~ (0,uhdlmap''))
      switchTab 0
    else liftIO $ Gtk.mainQuit
      
