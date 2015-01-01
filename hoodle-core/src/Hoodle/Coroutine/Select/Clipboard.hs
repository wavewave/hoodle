{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Select.Clipboard 
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Clipboard action while dealing with selection
-- 
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Select.Clipboard where

-- from other packages
import           Control.Applicative
import           Control.Lens (view,set,(.~))
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import qualified Data.Foldable as F
import           Data.Maybe (fromMaybe)
import qualified Graphics.UI.Gtk as Gtk
-- from hoodle-platform 
import           Data.Hoodle.Generic 
import           Data.Hoodle.Select
import           Data.Hoodle.Simple (Item(..))
import           Graphics.Hoodle.Render
import           Graphics.Hoodle.Render.Item
import           Graphics.Hoodle.Render.Type 
import           Graphics.Hoodle.Render.Type.HitTest
-- from this package 
import           Hoodle.Accessor
import           Hoodle.Coroutine.Draw
import           Hoodle.Coroutine.Commit 
import           Hoodle.Coroutine.Mode 
import           Hoodle.ModelAction.Page
import           Hoodle.ModelAction.Select
import           Hoodle.ModelAction.Select.Transform
import           Hoodle.ModelAction.Clipboard
import           Hoodle.Type.Alias
import           Hoodle.Type.Canvas 
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Event 
import           Hoodle.Type.PageArrangement 
import           Hoodle.Type.HoodleState 

-- |
updateTempHoodleSelectM :: CanvasId
                       -> Hoodle SelectMode 
                       -> Page SelectMode 
                       -> Int
                       -> MainCoroutine (Hoodle SelectMode)
updateTempHoodleSelectM cid thdl tpage pagenum = do   
  let newpage = hPage2RPage tpage
  callRenderer_ $ updatePageBuf cid newpage
  return (updateTempHoodleSelect thdl tpage pagenum)

 
-- |
deleteSelection :: MainCoroutine ()
deleteSelection = do 
  xst <- get
  let uhdl = view (unitHoodles.currentUnit) xst
      cid = getCurrentCanvasId uhdl
  case view hoodleModeState uhdl of
    SelectState thdl -> do 
      let Just (n,tpage) = view gselSelected thdl
          slayer = view (glayers.selectedLayer) tpage
      case unTEitherAlterHitted . view gitems $ slayer of 
        Left _ -> return () 
        Right alist -> do 
          let newlayer = Left . concat . getA $ alist
              newpage = set (glayers.selectedLayer) (GLayer (view gbuffer slayer) (TEitherAlterHitted newlayer)) tpage 
          newthdl <- updateTempHoodleSelectM cid thdl newpage n
          newuhdl <- liftIO . updatePageAll (SelectState newthdl) $ uhdl 
          let ui = view gtkUIManager xst
          liftIO $ toggleCutCopyDelete ui False 
          commit ((unitHoodles.currentUnit .~ newuhdl) xst)
          modeChange ToViewAppendMode 
          invalidateAll 
    _ -> return ()
 


-- | 
cutSelection :: MainCoroutine () 
cutSelection = copySelection >> deleteSelection

-- | 
copySelection :: MainCoroutine ()
copySelection = do 
    updateXState copySelectionAction >> invalidateAll 
  where copySelectionAction xst = 
          forBoth' unboxBiAct (fsingle xst) . view (unitHoodles.currentUnit.currentCanvasInfo) $ xst
        fsingle xst cinfo = do
            r <- runMaybeT $ do 
              let uhdl = view (unitHoodles.currentUnit) xst
                  hdlmodst = view hoodleModeState uhdl
                  epage = getCurrentPageEitherFromHoodleModeState cinfo hdlmodst
              pg <- (MaybeT . return . eitherMaybe) epage
              hitted <- (MaybeT . return . eitherMaybe) (rItmsInActiveLyr pg)
              (liftIO . updateClipboard xst . map rItem2Item . takeHitted) hitted
            return (fromMaybe xst r)
          where eitherMaybe (Left _) = Nothing
                eitherMaybe (Right a) = Just a 

-- |
getClipFromGtk :: MainCoroutine (Maybe [Item])
getClipFromGtk = do 
    doIOaction $ \evhandler -> do 
      hdltag <- liftIO $ Gtk.atomNew "hoodle"
      clipbd <- liftIO $ Gtk.clipboardGet hdltag
      liftIO $ Gtk.clipboardRequestText clipbd (callback4Clip evhandler)
      return (UsrEv ActionOrdered)
    waitSomeEvent (\case GotClipboardContent _ -> True; _ -> False ) >>= \(GotClipboardContent cnt') -> return cnt'


-- | 
pasteToSelection :: MainCoroutine () 
pasteToSelection = do 
    mitms <- getClipFromGtk
    F.forM_ mitms $ \itms -> do
      callRenderer $ GotRItems <$> mapM cnstrctRItem itms
      RenderEv (GotRItems ritms) <- 
        waitSomeEvent (\case RenderEv (GotRItems _) -> True; _ -> False)
      xst <- get
      cache <- renderCache 
      let ui = view gtkUIManager xst
      modeChange ToSelectMode 
      updateUhdl (pasteAction cache ui ritms)
      commit_ 
      invalidateAll  
  where 
    pasteAction cache ui itms uhdl = forBoth' unboxBiAct (fsimple cache ui itms uhdl) 
                                     . view currentCanvasInfo $ uhdl
    fsimple cache ui itms uhdl cinfo = do 
      geometry <- liftIO (getGeometry4CurrCvs uhdl)
      let cid = view canvasId cinfo
          pagenum = view currentPageNum cinfo 
          hdlmodst@(SelectState thdl) = view hoodleModeState uhdl
          nclipitms = adjustItemPosition4Paste geometry (PageNum pagenum) itms
          epage = getCurrentPageEitherFromHoodleModeState cinfo hdlmodst 
          tpage = either mkHPage id epage
          layerselect = view (glayers.selectedLayer) tpage 
          gbuf = view gbuffer layerselect
          newlayerselect = case rItmsInActiveLyr tpage of 
            Left nitms -> (GLayer gbuf . TEitherAlterHitted . Right) (nitms :- Hitted nclipitms :- Empty)
            Right alist -> (GLayer gbuf . TEitherAlterHitted . Right) 
                           (concat (interleave id unHitted alist) 
                            :- Hitted nclipitms 
                            :- Empty )
          tpage' = set (glayers.selectedLayer) newlayerselect tpage
      thdl' <- updateTempHoodleSelectM cid thdl tpage' pagenum 
      uhdl' <- liftIO $ updatePageAll (SelectState thdl') uhdl
      liftIO $ toggleCutCopyDelete ui True
      return uhdl'
