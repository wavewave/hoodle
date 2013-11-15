{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Dialog
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Dialog where

import           Control.Lens ((%~),view)
import           Control.Monad.Loops
import           Control.Monad.State
import           Graphics.UI.Gtk hiding (get,set)
import           System.Directory (getCurrentDirectory)
-- 
import           Control.Monad.Trans.Crtn
import           Control.Monad.Trans.Crtn.Event
import           Control.Monad.Trans.Crtn.Queue
--
import           Hoodle.Coroutine.Draw
import qualified Hoodle.Script.Coroutine as S
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Enum
import           Hoodle.Type.Event
import           Hoodle.Type.HoodleState
--

-- |
okMessageBox :: String -> MainCoroutine () 
okMessageBox msg = modify (tempQueue %~ enqueue action) 
                   >> waitSomeEvent (\case GotOk -> True ; _ -> False) 
                   >> return () 
  where 
    action = mkIOaction $ 
               \_evhandler -> do 
                 dialog <- messageDialogNew Nothing [DialogModal]
                   MessageQuestion ButtonsOk msg 
                 _res <- dialogRun dialog 
                 widgetDestroy dialog 
                 return (UsrEv GotOk)

-- | 
okCancelMessageBox :: String -> MainCoroutine Bool 
okCancelMessageBox msg = modify (tempQueue %~ enqueue action) 
                         >> waitSomeEvent p >>= return . q
  where 
    p (OkCancel _) = True 
    p _ = False 
    q (OkCancel b) = b 
    q _ = False 
    action = mkIOaction $ 
               \_evhandler -> do 
                 dialog <- messageDialogNew Nothing [DialogModal]
                   MessageQuestion ButtonsOkCancel msg 
                 res <- dialogRun dialog 
                 let b = case res of 
                           ResponseOk -> True
                           _ -> False
                 widgetDestroy dialog 
                 return (UsrEv (OkCancel b))

-- | 
fileChooser :: FileChooserAction -> Maybe String -> MainCoroutine (Maybe FilePath) 
fileChooser choosertyp mfname = do 
    mrecentfolder <- S.recentFolderHook 
    xst <- get 
    let rtrwin = view rootOfRootWindow xst 
    liftIO $ widgetQueueDraw rtrwin 
        
    modify (tempQueue %~ enqueue (action rtrwin mrecentfolder)) >> go 
  where 
    go = do r <- nextevent                   
            case r of 
              FileChosen b -> return b  
              UpdateCanvas cid -> -- this is temporary
                                  invalidateInBBox Nothing Efficient cid >> go  
              _ -> go 
    action win mrf = mkIOaction $ \_evhandler -> do 
      dialog <- fileChooserDialogNew Nothing (Just win) choosertyp 
                  [ ("OK", ResponseOk) 
                  , ("Cancel", ResponseCancel) ]
      case mrf of 
        Just rf -> fileChooserSetCurrentFolder dialog rf 
        Nothing -> getCurrentDirectory >>= fileChooserSetCurrentFolder dialog 
      maybe (return ()) (fileChooserSetCurrentName dialog) mfname 
      --   !!!!!! really hackish solution !!!!!!
      whileM_ (liftM (>0) eventsPending) (mainIterationDo False)
      
      res <- dialogRun dialog
      mr <- case res of 
              ResponseDeleteEvent -> return Nothing
              ResponseOk ->  fileChooserGetFilename dialog 
              ResponseCancel -> return Nothing 
              _ -> putStrLn "??? in fileOpen" >> return Nothing 
      widgetDestroy dialog
      return (UsrEv (FileChosen mr))
