{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hoodle.Coroutine.Network where

import Control.Concurrent hiding (yield)
import Control.Lens
import Control.Monad
import Control.Monad.Loops
import Control.Monad.State (get)
import Control.Monad.Trans
import Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.Binary as Bi
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word
import qualified Graphics.UI.Gtk as Gtk
import Hoodle.Coroutine.Draw
import Hoodle.Script.Hook
import Hoodle.Type.Coroutine
import Hoodle.Type.Enum
import Hoodle.Type.Event
import Hoodle.Type.HoodleState (hookSet)
import Network.Info
import Network.Simple.TCP

server :: (AllEvent -> IO ()) -> HostPreference -> T.Text -> IO ()
server evhandler ip txtorig = do
  ref <- newIORef txtorig
  forever $
    listen ip "4040" $ \(lsock, _) ->
      accept lsock $ \(sock, addr) -> do
        txt <- readIORef ref
        let bstr = TE.encodeUtf8 txt
            bstr_size :: Word32 = (fromIntegral . B.length) bstr
            bstr_size_binary = (mconcat . LB.toChunks . Bi.encode) bstr_size
        putStrLn $ "TCP connection established from " ++ show addr
        send sock (bstr_size_binary <> TE.encodeUtf8 txt)
        unfoldM_ $ do
          mbstr <- runMaybeT $ do
            bstr' <- MaybeT (recv sock 4)
            let getsize :: B.ByteString -> Word32
                getsize = Bi.decode . LB.fromChunks . return
                size = (fromIntegral . getsize) bstr'
                go s bs = do
                  liftIO $ putStrLn ("requested size = " ++ show s)
                  bstr1 <- MaybeT (recv sock s)
                  let s' = B.length bstr1
                  liftIO $ putStrLn ("obtained size = " ++ show s')
                  if s <= s'
                    then return (bs <> bstr1)
                    else go (s - s') (bs <> bstr1)
            go size B.empty
          forM_ mbstr $ \bstr' -> do
            let txt' = TE.decodeUtf8 bstr'
            (evhandler . UsrEv . NetworkProcess . NetworkReceived) txt'
            writeIORef ref txt'
          return mbstr
        putStrLn "FINISHED"

networkTextInputBody ::
  T.Text ->
  -- | (ip address,thread id,lock)
  MainCoroutine (String, ThreadId, MVar ())
networkTextInputBody txt = do
  mipscr <- runMaybeT $ do
    hkset <- MaybeT (view hookSet <$> lift get)
    (MaybeT . return) (getIPaddress hkset)
  let ipfind = do
        let ipv4num (IPv4 x) = x
            ismacnull (MAC a b c d e f) =
              a == 0 && b == 0 && c == 0
                && d
                == 0
                && e
                == 0
                && f
                == 0
        ifcs <- liftIO getNetworkInterfaces
        let ifcs2 =
              Prelude.filter (not . ismacnull . mac)
                . Prelude.filter ((0 /=) . ipv4num . ipv4)
                $ ifcs
        return (if Prelude.null ifcs2 then "127.0.0.1" else (show . ipv4 . head) ifcs2)
  ip <- maybe ipfind liftIO mipscr
  doIOaction $ \evhandler -> do
    done <- newEmptyMVar
    tid <- forkIO (server evhandler (Host ip) txt)
    (return . UsrEv . NetworkProcess) (NetworkInitialized tid done)
  let go = do
        r <- nextevent
        case r of
          UpdateCanvas cid -> invalidateInBBox Nothing Efficient cid >> go
          NetworkProcess (NetworkInitialized tid done) -> return (tid, done)
          _ -> go
  (tid, done) <- go
  return (ip, tid, done)

networkTextInput :: T.Text -> MainCoroutine (Maybe T.Text)
networkTextInput txt = do
  (ip, tid, done) <- networkTextInputBody txt
  let ipdialog msg _evhandler = do
        dialog <-
          Gtk.messageDialogNew
            Nothing
            [Gtk.DialogModal]
            Gtk.MessageQuestion
            Gtk.ButtonsOkCancel
            msg
        _ <- forkIO $ do
          readMVar done
          Gtk.dialogResponse dialog Gtk.ResponseOk
        res <- Gtk.dialogRun dialog
        let b = case res of
              Gtk.ResponseOk -> True
              _ -> False
        Gtk.widgetDestroy dialog
        return (UsrEv (OkCancel b))
  doIOaction (ipdialog ("networkedit " ++ ip ++ " 4040"))
  --
  let actf t = do
        r <- nextevent
        case r of
          UpdateCanvas cid ->
            invalidateInBBox Nothing Efficient cid
              >> actf t
          OkCancel True -> (return . Just) t
          OkCancel False -> return Nothing
          NetworkProcess (NetworkReceived txt') -> do
            doIOaction $ \_ ->
              Gtk.postGUISync (putMVar done ())
                >> (return . UsrEv . NetworkProcess) NetworkCloseDialog
            actf txt'
          _ -> actf t
  ntxt <- actf txt
  --
  doIOaction $ \_evhandler -> do
    killThread tid
    (return . UsrEv . NetworkProcess) NetworkClosed
  --
  return ntxt
