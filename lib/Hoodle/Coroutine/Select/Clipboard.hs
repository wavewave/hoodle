-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Select.Clipboard 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
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
import           Control.Concurrent
import           Control.Lens
import           Control.Monad.State 
import qualified Data.ByteString.Base64 as B64 
import qualified Data.ByteString.Char8 as C8
import           Data.IORef
import qualified Data.Serialize as Se 
import           Graphics.UI.Gtk hiding (get,set)
-- from hoodle-platform 
import           Control.Monad.Trans.Crtn
import           Control.Monad.Trans.Crtn.Event
import           Control.Monad.Trans.Crtn.Queue 
import           Data.Xournal.BBox
import           Data.Xournal.Generic 
import           Graphics.Xournal.Render.Type 
-- from this package 
import           Hoodle.Accessor
import           Hoodle.Coroutine.Draw
import           Hoodle.Coroutine.Commit 
import           Hoodle.Coroutine.Mode 
import           Hoodle.ModelAction.Page
import           Hoodle.ModelAction.Select
import           Hoodle.Script.Hook
import           Hoodle.Type.Canvas 
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Event 
import           Hoodle.Type.PageArrangement 
import           Hoodle.Type.XournalState 

-- |
deleteSelection :: MainCoroutine ()
deleteSelection = do 
  xstate <- get
  let SelectState txoj = view xournalstate xstate 
      Just (n,tpage) = gselectSelected txoj
      slayer = gselectedlayerbuf . glayers $ tpage
  case unTEitherAlterHitted . view g_bstrokes $ slayer of 
    Left _ -> return () 
    Right alist -> do 
      let newlayer = Left . concat . getA $ alist
          oldlayers = glayers tpage
          newpage = tpage { glayers = oldlayers { gselectedlayerbuf = GLayerBuf (view g_buffer slayer) (TEitherAlterHitted newlayer) } } 
      newtxoj <- liftIO $ updateTempXournalSelectIO txoj newpage n          
      newxstate <- liftIO $ updatePageAll (SelectState newtxoj) 
                            . set xournalstate (SelectState newtxoj)
                            $ xstate 
      commit newxstate 
      let ui = view gtkUIManager newxstate
      liftIO $ toggleCutCopyDelete ui False 
      invalidateAll 


-- | 
cutSelection :: MainCoroutine () 
cutSelection = copySelection >> deleteSelection

-- | 
copySelection :: MainCoroutine ()
copySelection = do 
    updateXState copySelectionAction >> invalidateAll 
  where copySelectionAction xst = 
          boxAction (fsingle xst) . view currentCanvasInfo $ xst
        fsingle xstate cinfo = maybe (return xstate) id $ do  
          let xojstate = view xournalstate xstate
          let epage = getCurrentPageEitherFromXojState cinfo xojstate
          eitherMaybe epage `pipe` getActiveLayer 
                            `pipe` (Right . liftIO . updateClipboard xstate . takeHittedStrokes)
          where eitherMaybe (Left _) = Nothing
                eitherMaybe (Right a) = Just a 
                x `pipe` a = x >>= eitherMaybe . a 
                infixl 6 `pipe`


-- | 
updateClipboard :: HoodleState -> [StrokeBBox] -> IO HoodleState 
updateClipboard xstate strs 
  | null strs = return xstate
  | otherwise = do 
    let ui = view gtkUIManager xstate
    hdltag <- atomNew "hoodle"
    -- tgttag <- atomNew "Stroke"
    -- seltag <- atomNew "Stroke"
    clipbd <- clipboardGet hdltag
    let bstr = C8.unpack . B64.encode . Se.encode $ strs 
    clipboardSetText clipbd bstr
    togglePaste ui True 
    case (view hookSet xstate) of 
      Nothing -> return () 
      Just hset -> case afterUpdateClipboardHook hset of 
                     Nothing -> return () 
                     Just uchook -> liftIO $ uchook strs 
    return xstate


-- |
callback4Clip :: (MyEvent -> IO ()) -> Maybe String -> IO ()
callback4Clip callbk Nothing = callbk (GotClipboardContent Nothing)
callback4Clip callbk (Just str) = do
    let r = do let bstr = C8.pack str 
               bstr' <- B64.decode bstr
               Se.decode bstr' 
    case r of 
      Left err -> callbk (GotClipboardContent Nothing)
      Right cnt -> callbk (GotClipboardContent (Just cnt))

-- |
getClipFromGtk :: MainCoroutine (Maybe [StrokeBBox])
getClipFromGtk = do 
    let action = Left . ActionOrder $ 
                   \evhandler -> do 
                       hdltag <- liftIO $ atomNew "hoodle"
                       clipbd <- liftIO $ clipboardGet hdltag
                       liftIO $ clipboardRequestText clipbd (callback4Clip evhandler)
                       return ActionOrdered 
    modify (tempQueue %~ enqueue action) 
--     return Nothing 
    go 
  where go = do r <- nextevent 
                case r of 
                  GotClipboardContent cnt' -> return cnt' 
                  _ -> go 

    
{-  
  
    hdltag <- liftIO $ atomNew "hoodle"
    clipbd <- liftIO $ clipboardGet hdltag
    ref <- liftIO $ newIORef Nothing 
    callbk <- view callBack <$> get     
    liftIO $ clipboardRequestText clipbd (callback4Clip callbk ref)
    cnt <- liftIO $ readIORef ref
    case cnt of 
      Nothing -> do 
        r <- nextevent 
        case r of 
          GotClipboardContent cnt' -> return cnt' 
          _ -> return Nothing 
      Just _ -> return cnt -}

-- | 
pasteToSelection :: MainCoroutine () 
pasteToSelection = do 
    mstrks <- getClipFromGtk 
    case mstrks of 
      Nothing -> return () 
      Just strks -> do 
        modeChange ToSelectMode >>updateXState (pasteAction strks) >> invalidateAll  
  where pasteAction stks xst = boxAction (fsimple stks xst) . view currentCanvasInfo 
                               $ xst
        fsimple stks xstate cinfo = do 
          geometry <- liftIO (getGeometry4CurrCvs xstate)
          let pagenum = view currentPageNum cinfo 
              xojstate@(SelectState txoj) = view xournalstate xstate
              nclipstrs = adjustStrokePosition4Paste geometry (PageNum pagenum) stks
              epage = getCurrentPageEitherFromXojState cinfo xojstate 
              tpage = either gcast id epage
              layerselect = gselectedlayerbuf . glayers $ tpage 
              ls  = glayers tpage
              gbuf = view g_buffer layerselect
              newlayerselect = case getActiveLayer tpage of 
                Left strs -> (GLayerBuf gbuf . TEitherAlterHitted . Right) (strs :- Hitted nclipstrs :- Empty)
                Right alist -> (GLayerBuf gbuf . TEitherAlterHitted . Right) 
                               (concat (interleave id unHitted alist) 
                                 :- Hitted nclipstrs 
                                 :- Empty )
              tpage' = tpage { glayers = ls { gselectedlayerbuf = newlayerselect } } 
          txoj' <- liftIO $ updateTempXournalSelectIO txoj tpage' pagenum 
          xstate' <- liftIO $ updatePageAll (SelectState txoj') 
                              . set xournalstate (SelectState txoj') 
                              $ xstate 
          commit xstate' 
          let ui = view gtkUIManager xstate' 
          liftIO $ toggleCutCopyDelete ui True
          return xstate' 
