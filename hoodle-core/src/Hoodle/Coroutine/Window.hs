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
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.UI.Gtk as Gtk
--
import           Data.Hoodle.Generic
import           Data.Hoodle.Simple (Dimension(..))
--
import           Hoodle.Accessor
import           Hoodle.Coroutine.Draw
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
    liftIO $ Gtk.notebookAppendPage notebook vboxcvs "test2"

    let wconf = Node 1
        initcvs = defaultCvsInfoSinglePage { _canvasId = 1 }
        initcvsbox = CanvasSinglePage initcvs
        -- cvs = (canvasWidgets.widgetConfig.doesUsePanZoomWidget .~ False)
        --       . (canvasWidgets.widgetConfig.doesUseLayerWidget .~ False)
        --       $ defaultCvsInfoSinglePage { _canvasId = 1 }
    uhdl' <- (undoTable .~ emptyUndo 50)   -- (undo = 50 for the time being) 
             . (frameState .~ wconf) 
             . updateFromCanvasInfoAsCurrentCanvas initcvsbox
             . (cvsInfoMap .~ M.empty)
             . (hoodleFileControl.hoodleFileName .~ Nothing) 
             . (unitKey .~ 1) <$> liftIO emptyUnitHoodle
        
    liftIO $ putStrLn "before constructFrame"
    -- (uhdl'',wdgt,_) <- liftIO $ constructFrame xst uhdl' wconf
    (cinfobox,canvas,scrwin) <- liftIO $ do
      let cid = 1
      canvas <- Gtk.drawingAreaNew
      scrwin <- Gtk.scrolledWindowNew Nothing Nothing 
      Gtk.containerAdd scrwin canvas
      hadj <- Gtk.adjustmentNew 0 0 500 100 200 200 
      vadj <- Gtk.adjustmentNew 0 0 500 100 200 200 
      Gtk.scrolledWindowSetHAdjustment scrwin hadj 
      Gtk.scrolledWindowSetVAdjustment scrwin vadj 
      let cinfo = CanvasInfo cid canvas Nothing scrwin (error "no viewInfo" :: ViewInfo a) 0 hadj vadj Nothing Nothing defaultCanvasWidgets Nothing 
          cinfobox = CanvasSinglePage cinfo
      return (cinfobox,canvas,scrwin)
    liftIO $ putStrLn "after constructFrame"

    liftIO $ do
      _exposeev <- canvas `Gtk.on` Gtk.exposeEvent $ Gtk.tryEvent $ do 
        liftIO $ Gtk.widgetGrabFocus canvas
        (liftIO . (xst^.callBack) . UsrEv) (UpdateCanvas 1) 
      return ()


    let wdgt = Gtk.castToWidget scrwin
    -- liftIO $ Gtk.containerAdd vboxcvs wdgt
 
    let uhdl'' = (currentCanvasInfo .~ cinfobox) uhdl'
        uhdl''' = (rootWindow .~ wdgt) . (rootContainer .~ Gtk.castToBox vboxcvs) $ uhdl''
        rtrwin = view rootOfRootWindow xst 
     
    liftIO $ putStrLn "before register"
    -- liftIO $ registerFrameToContainer rtrwin (Gtk.castToBox vboxcvs) wdgt
    liftIO $ do 
      Gtk.boxPackEnd vboxcvs wdgt Gtk.PackGrow 0 
      Gtk.widgetShowAll vboxcvs
      Gtk.widgetShowAll notebook
      Gtk.widgetShowAll rtrwin
      Gtk.widgetQueueDraw rtrwin


    liftIO $ putStrLn "after register"
    liftIO $ Gtk.widgetShowAll notebook

    modify $ (unitHoodles._2.at 2 .~ Just uhdl''')

    liftIO $ putStrLn "before invalidate"
    invalidateAll
    liftIO $ putStrLn "after invalidate"
  

-- |
nextTab :: MainCoroutine ()
nextTab = do
    uhdls <- view unitHoodles <$> get
    let current = fst uhdls
        lst = filter ((>current).fst) (M.assocs (snd uhdls))
    when ((not.null) lst) $ do
      let uhdl = snd (head lst)
      modify $ (unitHoodles.currentUnit .~ uhdl)
      invalidateAll
{-    
      liftIO $ print "nextTab"
          cinfobox = getCanvasInfo 1 uhdl
          fsingle :: CanvasInfo a -> IO ()
          fsingle cinfo = do
            let canvas = view drawArea cinfo
            win <- Gtk.widgetGetDrawWindow canvas
            Gtk.renderWithDrawable win $ do
              Cairo.rectangle 0 0 100 100
              Cairo.fill
            print (view currentPageNum cinfo)
      liftIO $ forBoth' unboxBiAct fsingle cinfobox -}
      {- 
      geometry <- liftIO $ getCanvasGeometryCvsId 1 uhdl
      invalidateGeneral 1 Nothing Clear
        (drawSinglePage geometry) (drawSinglePageSel geometry) (drawContHoodle geometry) (drawContHoodleSel geometry) -}
      

