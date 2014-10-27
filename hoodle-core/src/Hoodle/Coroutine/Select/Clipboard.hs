{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Select.Clipboard 
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
--
-- License     : GPL-3
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
import           Control.Lens (view,set,(%~),(.~))
import           Control.Monad.State 
import           Graphics.UI.Gtk hiding (get,set)
-- from hoodle-platform 
import           Control.Monad.Trans.Crtn.Queue 
import           Data.Hoodle.Generic 
import           Data.Hoodle.Select
import           Data.Hoodle.Simple
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
import           Hoodle.Type.Canvas 
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Event 
import           Hoodle.Type.PageArrangement 
import           Hoodle.Type.HoodleState 
 
-- |
deleteSelection :: MainCoroutine ()
deleteSelection = do 
  xst <- get
  let uhdl = view (unitHoodles.currentUnit) xst
  case view hoodleModeState uhdl of
    SelectState thdl -> do 
      let Just (n,tpage) = view gselSelected thdl
          slayer = view (glayers.selectedLayer) tpage
      case unTEitherAlterHitted . view gitems $ slayer of 
        Left _ -> return () 
        Right alist -> do 
          let newlayer = Left . concat . getA $ alist
              newpage = set (glayers.selectedLayer) (GLayer (view gbuffer slayer) (TEitherAlterHitted newlayer)) tpage 
              cache = view renderCache xst
          newthdl <- liftIO $ updateTempHoodleSelectIO cache thdl newpage n          
          newuhdl <- liftIO $ updatePageAll (SelectState newthdl) 
                              . set hoodleModeState (SelectState newthdl)
                              $ uhdl
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
        fsingle xst cinfo = maybe (return xst) id $ do  
          let uhdl = view (unitHoodles.currentUnit) xst
              hdlmodst = view hoodleModeState uhdl
              epage = getCurrentPageEitherFromHoodleModeState cinfo hdlmodst
          eitherMaybe epage `pipe` rItmsInActiveLyr 
                            `pipe` (Right . liftIO . updateClipboard xst . map rItem2Item . takeHitted)
          where eitherMaybe (Left _) = Nothing
                eitherMaybe (Right a) = Just a 
                x `pipe` a = x >>= eitherMaybe . a 
                infixl 6 `pipe`

-- |
getClipFromGtk :: MainCoroutine (Maybe [Item])
getClipFromGtk = do 
    doIOaction $ \evhandler -> do 
      hdltag <- liftIO $ atomNew "hoodle"
      clipbd <- liftIO $ clipboardGet hdltag
      liftIO $ clipboardRequestText clipbd (callback4Clip evhandler)
      return (UsrEv ActionOrdered)
    waitSomeEvent (\case GotClipboardContent _ -> True; _ -> False ) >>= \(GotClipboardContent cnt') -> return cnt'

  -- where go = do r <- nextevent 
  --               case r of 
  --                 GotClipboardContent cnt' -> return cnt' 
  --                 _ -> go 


-- | 
pasteToSelection :: MainCoroutine () 
pasteToSelection = do 
    mitms <- getClipFromGtk 

    liftIO $ print mitms
    case mitms of 
      Nothing -> return () 
      Just itms -> do 
        callRenderer $ GotRItems <$> mapM cnstrctRItem itms
        RenderEv (GotRItems ritms) <- waitSomeEvent (\case RenderEv (GotRItems _) -> True; _ -> False)
        xst <- get
        let ui = view gtkUIManager xst
            cache = view renderCache xst
        modeChange ToSelectMode >> updateUhdl (pasteAction cache ui ritms) >> commit_ >> invalidateAll  
  where 
    pasteAction cache ui itms uhdl = forBoth' unboxBiAct (fsimple cache ui itms uhdl) 
                                     . view currentCanvasInfo $ uhdl
    fsimple cache ui itms uhdl cinfo = do 
      geometry <- liftIO (getGeometry4CurrCvs uhdl)
      let pagenum = view currentPageNum cinfo 
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
      thdl' <- liftIO $ updateTempHoodleSelectIO cache thdl tpage' pagenum 
      uhdl' <- liftIO $ updatePageAll (SelectState thdl') 
                 . set hoodleModeState (SelectState thdl') 
                 $ uhdl
      liftIO $ toggleCutCopyDelete ui True
      return uhdl'
