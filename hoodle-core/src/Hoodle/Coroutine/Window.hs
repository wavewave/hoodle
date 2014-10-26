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
import           Control.Lens (view,set,over,(.~))
import           Control.Monad.State 
import qualified Data.IntMap as M
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
        let rtwin = view rootWindow uhdl
            rtcntr = view rootContainer uhdl
            rtrwin = view rootOfRootWindow xst 
        liftIO $ Gtk.containerRemove rtcntr rtwin
        (uhdl',win,fstate'') <- liftIO $ constructFrame' xst cinfobox uhdl fstate'
        let uhdl'' = ((frameState .~ fstate'') . (rootWindow .~ win)) uhdl'
        let xst3 = (unitHoodles.currentUnit .~ uhdl'') xst
        put xst3 
        liftIO $ registerFrameToContainer rtrwin rtcntr win
        -- liftIO $ boxPackEnd rtcntr win PackGrow 0 
        -- liftIO $ widgetShowAll rtcntr  
        -- liftIO $ widgetQueueDraw rtrwin
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
        -- liftIO $ Gtk.boxPackEnd rtcntr win PackGrow 0 
        -- liftIO $ Gtk.widgetShowAll rtcntr  
        -- liftIO $ Gtk.widgetQueueDraw rtrwin
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
      PaneMoveEnd -> do 
        return () 
      CanvasConfigure cid w' h'-> do 
        canvasConfigureGenUpdate canvasZoomUpdateBufAll cid (CanvasDimension (Dim w' h')) 
        >> paneMoveStart
      _ -> paneMoveStart
       

-- | not yet implemented?
paneMoved :: MainCoroutine () 
paneMoved = do 
    liftIO $ putStrLn "pane moved called"
    
-- | start full screen mode
fullScreen :: MainCoroutine ()
fullScreen = do 
    xst <- get 
    let b = view isFullScreen xst
        rwin = view rootOfRootWindow xst 
    if b 
      then do 
        liftIO $ Gtk.windowUnfullscreen rwin
        modify (over isFullScreen (const False))
      else do 
        liftIO $ Gtk.windowFullscreen rwin 
        modify (over isFullScreen (const True))

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
    (uhdl'',wdgt,_) <- liftIO $ constructFrame xst uhdl' wconf
    liftIO $ putStrLn "after constructFrame"

    liftIO $ Gtk.containerAdd vboxcvs wdgt 
 
    let uhdl''' = (rootWindow .~ wdgt) . (rootContainer .~ Gtk.castToBox vboxcvs) $ uhdl''
        rtrwin = view rootOfRootWindow xst 

        -- let rtwin = view rootWindow uhdl
        --     rtcntr = view rootContainer uhdl
    liftIO $ putStrLn "before register"
    liftIO $ registerFrameToContainer rtrwin (Gtk.castToBox vboxcvs) wdgt
    liftIO $ putStrLn "after register"

    modify $ (unitHoodles.currentUnit .~ uhdl''')

    liftIO $ putStrLn "before invalidate"
    invalidateAll
    liftIO $ putStrLn "after invalidate"
{-
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
        let rtwin = view rootWindow uhdl
            rtcntr = view rootContainer uhdl
            rtrwin = view rootOfRootWindow xst 
        liftIO $ containerRemove rtcntr rtwin
        (uhdl',win,fstate'') <- liftIO $ constructFrame' xst cinfobox uhdl fstate'
        let uhdl'' = ((frameState .~ fstate'') . (rootWindow .~ win)) uhdl'
        let xst3 = (unitHoodles.currentUnit .~ uhdl'') xst
        put xst3 
        liftIO $ registerFrameToContainer rtrwin rtcntr win
        -- liftIO $ boxPackEnd rtcntr win PackGrow 0 
        -- liftIO $ widgetShowAll rtcntr  
        -- liftIO $ widgetQueueDraw rtrwin
        (uhdl3,_wconf) <- liftIO $ eventConnect xst3 uhdl'' (view frameState uhdl'')
        updateUhdl $ const (liftIO $ updatePageAll (view hoodleModeState uhdl3) uhdl3)
        canvasZoomUpdateAll
        invalidateAll 


    liftIO $ do
      lbtn <- buttonNewWithLabel "test" 
      notebookAppendPage notebook lbtn "test2"
      widgetShowAll notebook
      putStrLn "add tab called"
           
  -}  
  
  

