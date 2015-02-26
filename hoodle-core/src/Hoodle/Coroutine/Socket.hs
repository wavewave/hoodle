{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Socket
-- Copyright   : (c) 2014, 2015 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Socket where

import           Control.Applicative
import           Control.Concurrent
import qualified Control.Exception as E
import           Control.Lens (view)
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Maybe
import           Data.Aeson.Parser
import           Data.Aeson.Types
import qualified Data.Attoparsec.ByteString as A
import qualified Data.CaseInsensitive as CI
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time.Clock
import qualified Graphics.UI.Gtk as Gtk
import Network.HTTP.Client.Internal (computeCookieString)
import Network.HTTP.Conduit
import qualified Network.WebSockets as WS
import System.Directory
import System.FilePath ((</>))
--
import Hoodle.Coroutine.Dialog
import Hoodle.Coroutine.Hub.Common
import Hoodle.Script.Hook
import Hoodle.Type.Coroutine
import Hoodle.Type.Event
import Hoodle.Type.Hub
import Hoodle.Type.HoodleState
import Hoodle.Type.Synchronization
import Hoodle.Util
--

data Permission = Owner | Editor | Viewer | Stranger deriving (Show,Eq,Ord,Read)

instance ToJSON Permission where
  toJSON Owner    = String "owner"
  toJSON Editor   = String "editor"
  toJSON Viewer   = String "viewer"
  toJSON Stranger = String "stranger"

instance FromJSON Permission where
  parseJSON (String t) =
    case t of
      "owner"    -> return Owner
      "editor"   -> return Editor
      "viewer"   -> return Viewer
      "stranger" -> return Stranger
      _ -> fail "error in parsing Permission"
  parseJSON _ = fail "error in parsing Permission"

data HoodleWSEvent = HWSOpen { hws_permission :: Permission
                             , hws_fileuuid   :: T.Text
                             , hws_filepath   :: T.Text }
                   | HWSSync { hws_clientuuid :: T.Text
                             , hws_filesyncstatus :: FileSyncStatus
                             } 
               deriving Show

instance ToJSON HoodleWSEvent where
  toJSON HWSOpen {..} = object [ "eventType"  .= toJSON ("open" :: T.Text)
                               , "permission" .= toJSON hws_permission
                               , "fileuuid"   .= toJSON hws_fileuuid
                               , "filepath"   .= toJSON hws_filepath ]
  toJSON HWSSync {..} = object [ "eventType"  .= toJSON ("sync" :: T.Text)
                               , "clientuuid" .= toJSON hws_clientuuid
                               , "filesyncstatus"   .= toJSON hws_filesyncstatus
                               ]

instance FromJSON HoodleWSEvent where
  parseJSON (Object v) = do
      typ :: T.Text <- v .: "eventType"
      case typ of
        "open" -> HWSOpen <$> v .: "permission" <*> v .: "fileuuid" <*> v .: "filepath" 
        "sync" -> HWSSync <$> v .: "clientuuid" <*> v .: "filesyncstatus"
        _ -> fail "error in parsing HoodleWSEvent"
  parseJSON _ = fail "error in parsing HoodleWSEvent"

-- |
socketConnect :: MainCoroutine ()
socketConnect = do
    xst <- get
    r <- runMaybeT $ do 
      hset <- (MaybeT . return) $ view hookSet xst
      hinfo <- (MaybeT . return) (hubInfo hset)
      lift (hoodleWSStart hinfo)
    case r of 
      Nothing -> okMessageBox "socket connect not successful" >> return ()
      Just _ -> return ()  


hoodleWSStart :: HubInfo -> MainCoroutine ()
hoodleWSStart hinfo@HubInfo {..} = do
    hdir <- liftIO $ getHomeDirectory
    let tokfile = hdir </> ".hoodle.d" </> "token.txt"
    prepareToken hinfo tokfile 
    doIOaction $ \evhandler -> do 
      forkIO $ (`E.catch` (\(err:: E.SomeException)-> print err >> return ())) $ 
        withHub hinfo tokfile $ \_manager coojar -> do
          request2' <- parseUrl ("http://" <> hubsocketurl <> ":" <> show hubsocketport </> hubsocketpath)
          ctime <- liftIO getCurrentTime
          let (bstr,_) = computeCookieString request2' coojar ctime True
              newheaders = [(CI.mk "Cookie",bstr)] 
          liftIO $ WS.runClientWith hubsocketurl hubsocketport hubsocketpath WS.defaultConnectionOptions newheaders $ \conn -> forever $ do 
            txt <- WS.receiveData conn
            runEitherT $ do
              v <- hoistEither $ A.parseOnly json (TE.encodeUtf8 txt)
              o :: HoodleWSEvent <- hoistEither $ parseEither parseJSON v
              lift $ hoodleWSDispatchEvent evhandler hinfo o 
      return (UsrEv ActionOrdered)

hoodleWSDispatchEvent :: (AllEvent -> IO ()) -> HubInfo -> HoodleWSEvent -> IO ()
hoodleWSDispatchEvent evhandler HubInfo {..} HWSOpen {..} = do
    let urlpath = FileUrl (hubfileroot </> T.unpack hws_filepath)
        uuid = read (T.unpack hws_fileuuid)
    case hws_permission of
      Owner  -> (Gtk.postGUIAsync . evhandler . UsrEv) (OpenLink urlpath Nothing)
      Editor -> (Gtk.postGUIAsync . evhandler . UsrEv) (OpenShared uuid)
      _ -> return ()
hoodleWSDispatchEvent evhandler HubInfo {..} HWSSync {..} = 
    let clientuuid = read (T.unpack hws_clientuuid) 
    in (Gtk.postGUIAsync . evhandler . UsrEv) (GotSyncEvent False clientuuid hws_filesyncstatus)
