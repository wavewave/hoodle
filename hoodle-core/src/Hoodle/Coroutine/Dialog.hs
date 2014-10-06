{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Dialog
-- Copyright   : (c) 2013,2014 Ian-Woo Kim
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
import qualified Data.Foldable as F
import qualified Graphics.UI.Gtk as Gtk
import           System.Directory (getCurrentDirectory)
-- 
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
                 dialog <- Gtk.messageDialogNew Nothing [Gtk.DialogModal]
                   Gtk.MessageQuestion Gtk.ButtonsOk msg 
                 _res <- Gtk.dialogRun dialog 
                 Gtk.widgetDestroy dialog 
                 return (UsrEv GotOk)


-- |
longTextMessageBox :: String -> MainCoroutine () 
longTextMessageBox msg = modify (tempQueue %~ enqueue action) 
                         >> waitSomeEvent (\case GotOk -> True ; _ -> False) 
                         >> return () 
  where 
    action = mkIOaction $ 
               \_evhandler -> do 
                 dialog <- Gtk.dialogNew

                 vbox <- Gtk.dialogGetUpper dialog
                 hbox <- Gtk.hBoxNew False 0
                 txtbuf <- Gtk.textBufferNew Nothing
                 Gtk.textBufferSetText txtbuf msg
                 txtview <- Gtk.textViewNewWithBuffer txtbuf
                 vadj <- Gtk.textViewGetVadjustment txtview
                 vscr <- Gtk.vScrollbarNew vadj
                 Gtk.widgetSetSizeRequest txtview 400 700
                 Gtk.boxPackEnd hbox vscr Gtk.PackNatural 0 
                 Gtk.boxPackStart hbox txtview Gtk.PackGrow 0
                 Gtk.boxPackStart vbox hbox Gtk.PackGrow 0
                 

                 _btnOk <- Gtk.dialogAddButton dialog ("Ok" :: String) Gtk.ResponseOk

                 Gtk.widgetShowAll dialog
                 _res <- Gtk.dialogRun dialog 
                 Gtk.widgetDestroy dialog 
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
                 dialog <- Gtk.messageDialogNew Nothing [Gtk.DialogModal]
                   Gtk.MessageQuestion Gtk.ButtonsOkCancel msg 
                 res <- Gtk.dialogRun dialog 
                 let b = case res of 
                           Gtk.ResponseOk -> True
                           _ -> False
                 Gtk.widgetDestroy dialog 
                 return (UsrEv (OkCancel b))

-- | 
fileChooser :: Gtk.FileChooserAction -> Maybe String -> MainCoroutine (Maybe FilePath) 
fileChooser choosertyp mfname = do 
    mrecentfolder <- S.recentFolderHook 
    xst <- get 
    let rtrwin = view rootOfRootWindow xst 
    liftIO $ Gtk.widgetQueueDraw rtrwin 
    modify (tempQueue %~ enqueue (action rtrwin mrecentfolder)) >> go 
  where 
    go = do r <- nextevent                   
            case r of 
              FileChosen b -> return b  
              UpdateCanvas cid -> -- this is temporary
                                  invalidateInBBox Nothing Efficient cid >> go  
              _ -> go 
    action win mrf = mkIOaction $ \_evhandler -> do 
      dialog <- Gtk.fileChooserDialogNew Nothing (Just win) choosertyp 
                  [ ("OK", Gtk.ResponseOk) 
                  , ("Cancel", Gtk.ResponseCancel) ]
      case mrf of 
        Just rf -> Gtk.fileChooserSetCurrentFolder dialog rf 
        Nothing -> getCurrentDirectory >>= Gtk.fileChooserSetCurrentFolder dialog 
      F.mapM_ (Gtk.fileChooserSetCurrentName dialog) mfname 
      --   !!!!!! really hackish solution !!!!!!
      whileM_ (liftM (>0) Gtk.eventsPending) (Gtk.mainIterationDo False)
      
      res <- Gtk.dialogRun dialog
      mr <- case res of 
              Gtk.ResponseDeleteEvent -> return Nothing
              Gtk.ResponseOk ->  Gtk.fileChooserGetFilename dialog 
              Gtk.ResponseCancel -> return Nothing 
              _ -> putStrLn "??? in fileOpen" >> return Nothing 
      Gtk.widgetDestroy dialog
      return (UsrEv (FileChosen mr))
