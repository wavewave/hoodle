{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Window 
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Window where

import           Control.Applicative
import           Control.Lens (view,set,over,(^.),(.~),_2,at)
import           Control.Monad.State 
import qualified Data.IntMap as M
import           Data.Maybe
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.UI.Gtk as Gtk
--
import           Data.Hoodle.Generic
import           Data.Hoodle.Simple (Dimension(..), defaultHoodle)
import           Graphics.Hoodle.Render (cnstrctRHoodle)
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
import           Hoodle.View.Draw
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
        updateUhdl $ \uhdl -> do
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
        updateUhdl $ \uhdl -> do
          (uhdl',_wconf) <- liftIO $ eventConnect xst3 uhdl (view frameState uhdl)
          liftIO $ updatePageAll (view hoodleModeState uhdl') uhdl'
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
addTab :: MainCoroutine ()
addTab = do
    xst <- get
    notebook <- view rootNotebook <$> get
    
    vboxcvs <- liftIO $ Gtk.vBoxNew False 0
    tabnum <- liftIO $ Gtk.notebookAppendPage notebook vboxcvs "undefined"

    let wconf = Node 1
        initcvs = defaultCvsInfoSinglePage { _canvasId = 1 }
        initcvsbox = CanvasSinglePage initcvs
    uhdl' <- (undoTable .~ emptyUndo 50)   -- (undo = 50 for the time being) 
             . (frameState .~ wconf) 
             . updateFromCanvasInfoAsCurrentCanvas initcvsbox
             . (cvsInfoMap .~ M.empty)
             . (hoodleFileControl.hoodleFileName .~ Nothing) 
             . (unitKey .~ tabnum) <$> liftIO emptyUnitHoodle
    -- liftIO $ print (M.keys (view cvsInfoMap uhdl'))
          
    uhdl4 <- liftIO $ do
      (uhdl'',wdgt,_) <- liftIO $ constructFrame xst uhdl' wconf
      let uhdl3 = (rootWindow .~ wdgt) . (rootContainer .~ Gtk.castToBox vboxcvs) $ uhdl''
      (uhdl4,_wconf) <- eventConnect xst uhdl3 (view frameState uhdl3)
      registerFrameToContainer (xst^.rootOfRootWindow) (Gtk.castToBox vboxcvs) wdgt
      Gtk.widgetShowAll notebook
      return uhdl4
      -- return (UsrEv (NewUnitHoodle uhdl4))
    -- NewUnitHoodle uhdl4 <- waitSomeEvent (\case NewUnitHoodle _ -> True; _ -> False)

    doIOaction_ $ 
      blockWhile (view (uiComponentSignalHandler.switchTabSignal) xst) $
        Gtk.set notebook [Gtk.notebookPage Gtk.:= tabnum]


    (liftIO defaultHoodle) >>= \hdl' -> callRenderer $ cnstrctRHoodle hdl' >>= return . GotRHoodle
    RenderEv (GotRHoodle rhdl) <- waitSomeEvent (\case RenderEv (GotRHoodle _) -> True; _ -> False)
    let uhdl5 = (hoodleModeState .~ ViewAppendState rhdl) uhdl4
        current = xst ^. unitHoodles.currentUnit

    
    -- modify $ (unitHoodles._2.at tabnum .~ Just uhdl5)
    modify $ (unitHoodles.currentUnit .~ uhdl5)
    -- viewModeChange ToContSinglePage
    updateUhdl $ \uhdl -> do
      liftIO (updatePageAll (view hoodleModeState uhdl) uhdl)
      unboxBiAct (sing2ContPage uhdl) (const (return uhdl)) . view currentCanvasInfo $ uhdl
    pageZoomChange FitWidth
    invalidateAll 
  
    -- modify $ (unitHoodles.currentUnit .~ current)
    -- modify $ (unitHoodles.currentUnit .~ uhdl4)
    -- getFileContent Nothing
    -- canvasZoomUpdateAll
    
 

  

-- |
nextTab :: MainCoroutine ()
nextTab = do
    xst <- get
    uhdls <- view unitHoodles <$> get
    let current = fst uhdls
        lst = filter ((>current).fst) (M.assocs (snd uhdls))
    when ((not.null) lst) $ do
      let uhdl = snd (head lst)
          tabnum = uhdl^.unitKey
          notebook = view rootNotebook xst
      doIOaction_ $ Gtk.set notebook [Gtk.notebookPage Gtk.:= tabnum]
      modify $ (unitHoodles.currentUnit .~ uhdl)
      updateUhdl $ \uhdl -> liftIO (updatePageAll (view hoodleModeState uhdl) uhdl)
      invalidateAll 

-- |
switchTab :: Int -> MainCoroutine ()
switchTab tabnum = do
    liftIO $ putStrLn ("switch to " ++ show tabnum)
    xst <- get
    let notebook = view rootNotebook xst
    doIOaction_ $ Gtk.set notebook [Gtk.notebookPage Gtk.:= tabnum ]

    uhdls <- view unitHoodles <$> get
    let current = fst uhdls
        ks = M.keys (snd uhdls)
    liftIO $ print ks
    liftIO $ print current
    when (tabnum /= current) $ do 
      let uhdl = fromJust (M.lookup tabnum (snd uhdls))
      
      modify $ (unitHoodles.currentUnit .~ uhdl)
      updateUhdl $ \uhdl -> liftIO (updatePageAll (view hoodleModeState uhdl) uhdl)
      view currentCanvasInfo uhdl # 
        forBoth' unboxBiAct $ \cinfo -> do
          (w,h) <- liftIO $ Gtk.widgetGetSize (cinfo^.drawArea) 
          doCanvasConfigure (cinfo^.canvasId) (CanvasDimension (Dim (fromIntegral w) (fromIntegral h)))
      -- canvasZoomUpdateAll
      invalidateAll 

