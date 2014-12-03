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

import           Control.Applicative ((<$>),(<*>))
import           Control.Lens (view)
import           Control.Monad.Loops
import           Control.Monad.State
import qualified Data.Foldable as F
import qualified Data.Text as T
import qualified Graphics.UI.Gtk as Gtk
import           System.Directory (getCurrentDirectory)
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
okMessageBox msg = action >> waitSomeEvent (\case GotOk -> True ; _ -> False) >> return () 
  where 
    action = doIOaction $ 
               \_evhandler -> do 
                 dialog <- Gtk.messageDialogNew Nothing [Gtk.DialogModal]
                   Gtk.MessageQuestion Gtk.ButtonsOk msg 
                 _res <- Gtk.dialogRun dialog 
                 Gtk.widgetDestroy dialog 
                 return (UsrEv GotOk)


-- | 
okCancelMessageBox :: String -> MainCoroutine Bool 
okCancelMessageBox msg = action >> waitSomeEvent p >>= return . q
  where 
    p (OkCancel _) = True 
    p _ = False 
    q (OkCancel b) = b 
    q _ = False 
    action = doIOaction $ 
               \_evhandler -> do 
                 dialog <- Gtk.messageDialogNew Nothing [Gtk.DialogModal]
                   Gtk.MessageQuestion Gtk.ButtonsOkCancel msg 
                 res <- Gtk.dialogRun dialog 
                 let b = case res of 
                           Gtk.ResponseOk -> True
                           _ -> False
                 Gtk.widgetDestroy dialog 
                 return (UsrEv (OkCancel b))


-- | single line text input : almost abandoned now
textInputDialog :: String -> MainCoroutine (Maybe String) 
textInputDialog msg = do 
    doIOaction $ \_evhandler -> do 
                   dialog <- Gtk.messageDialogNew Nothing [Gtk.DialogModal]
                     Gtk.MessageQuestion Gtk.ButtonsOkCancel msg
                   vbox <- Gtk.dialogGetUpper dialog
                   txtvw <- Gtk.textViewNew
                   Gtk.boxPackStart vbox txtvw Gtk.PackGrow 0 
                   Gtk.widgetShowAll dialog
                   res <- Gtk.dialogRun dialog 
                   case res of 
                     Gtk.ResponseOk -> do 
                       buf <- Gtk.textViewGetBuffer txtvw 
                       (istart,iend) <- (,) <$> Gtk.textBufferGetStartIter buf
                                            <*> Gtk.textBufferGetEndIter buf
                       l <- Gtk.textBufferGetText buf istart iend True
                       Gtk.widgetDestroy dialog
                       return (UsrEv (TextInput (Just l)))
                     _ -> do 
                       Gtk.widgetDestroy dialog
                       return (UsrEv (TextInput Nothing))
    TextInput input <- waitSomeEvent (\case TextInput _ -> True ; _ -> False)
    return input 



-- | 
keywordDialog :: [T.Text] -> MainCoroutine (Maybe T.Text)
keywordDialog keylst = do
    doIOaction (keywordDialog' keylst)
    keywordLoop

-- |
keywordDialog' :: [T.Text] -> (AllEvent -> IO ()) -> IO AllEvent
keywordDialog' keys = \_evhandler -> do
    dialog <- Gtk.dialogNew
    vbox <- Gtk.dialogGetUpper dialog
    hbox <- Gtk.hBoxNew False 0
    Gtk.boxPackStart vbox hbox Gtk.PackNatural 0
    _btnOk <- Gtk.dialogAddButton dialog ("Ok" :: String) Gtk.ResponseOk
    _btnCancel <- Gtk.dialogAddButton dialog ("Cancel" :: String) Gtk.ResponseCancel
    cbx <- Gtk.comboBoxNewText 
    klst <- mapM (Gtk.comboBoxAppendText cbx) keys
    when ((not.null) klst) $ 
      Gtk.comboBoxSetActive cbx (head klst)
    Gtk.boxPackStart hbox cbx Gtk.PackGrow 2
    Gtk.widgetShowAll dialog
    res <- Gtk.dialogRun dialog
    Gtk.widgetDestroy dialog
    case res of 
      Gtk.ResponseOk -> do
        keystr <- Gtk.comboBoxGetActiveText cbx
        (return . UsrEv . Keyword) keystr
      Gtk.ResponseCancel -> return (UsrEv (Keyword Nothing))
      _ -> return (UsrEv (Keyword Nothing))

-- | main event loop for keyword dialog
keywordLoop :: MainCoroutine (Maybe T.Text)
keywordLoop = waitSomeEvent (\case Keyword _ -> True ; _ -> False ) >>= \(Keyword x) -> return x


-- |
longTextMessageBox :: String -> MainCoroutine () 
longTextMessageBox msg = action
                         >> waitSomeEvent (\case GotOk -> True ; _ -> False) 
                         >> return () 
  where 
    action = doIOaction $ 
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
fileChooser :: Gtk.FileChooserAction -> Maybe String -> MainCoroutine (Maybe FilePath) 
fileChooser choosertyp mfname = do 
    mrecentfolder <- S.recentFolderHook 
    xst <- get 
    let rtrwin = view rootOfRootWindow xst 
    liftIO $ Gtk.widgetQueueDraw rtrwin 
    doIOaction (action rtrwin mrecentfolder) >> go
  where 
    go = do r <- nextevent                   
            case r of 
              FileChosen b -> return b  
              UpdateCanvas cid -> -- this is temporary
                                  invalidateInBBox Nothing Efficient cid >> go  
              _ -> go 
    action win mrf _evhandler = do 
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
