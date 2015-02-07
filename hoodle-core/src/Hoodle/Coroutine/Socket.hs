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
import qualified Data.Aeson.Encode as AE
import           Data.Aeson.Parser
import           Data.Aeson.Types
import qualified Data.Attoparsec.ByteString as A
import qualified Data.CaseInsensitive as CI
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time.Clock
import qualified Graphics.UI.Gtk as Gtk
import Network
import Network.Google.OAuth2 (formUrl, exchangeCode, refreshTokens,
                               OAuth2Client(..), OAuth2Tokens(..))
import Network.HTTP.Client.Internal (computeCookieString)
import Network.HTTP.Conduit
import qualified Network.WebSockets as WS
import System.Directory
import System.Exit    (ExitCode(..))
import System.FilePath ((</>))
import System.Info (os)
import System.Process (rawSystem)
--
import Hoodle.Coroutine.Dialog
-- import Hoodle.Coroutine.HubInternal
import Hoodle.Coroutine.Hub.Common
import Hoodle.Script.Hook
import Hoodle.Type.Coroutine
import Hoodle.Type.Event
import Hoodle.Type.Hub
import Hoodle.Type.HoodleState
import Hoodle.Util
--

data HoodleWSEvent = HWSOpen { hws_filepath :: T.Text }
                   | HWSSync { hws_fileuuid :: T.Text 
                             , hws_clientuuid :: T.Text }
                   deriving Show

instance ToJSON HoodleWSEvent where
  toJSON HWSOpen {..} = object [ "eventType" .= toJSON ("open" :: T.Text) 
                               , "filepath"  .= toJSON hws_filepath ]

instance FromJSON HoodleWSEvent where
  parseJSON (Object v) =
    let r = do 
          String typ <- HM.lookup "eventType" v
          case typ of
            "open" -> do String fp <- HM.lookup "filepath" v
                         return HWSOpen { hws_filepath = fp }
            "sync" -> do String fileuuid <- HM.lookup "fileuuid" v
                         String clientuuid <- HM.lookup "clientuuid" v
                         return HWSSync { hws_fileuuid = fileuuid 
                                        , hws_clientuuid = clientuuid }
            _ -> Nothing
    in case r of
         Nothing -> fail "error in parsing HoodleWSEvent"
         Just result -> return result
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
        withHub hinfo tokfile $ \manager coojar -> do
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
    (Gtk.postGUIAsync . evhandler . UsrEv) (OpenLink urlpath Nothing)
hoodleWSDispatchEvent evhandler HubInfo {..} HWSSync {..} = do
    let fileuuid = read (T.unpack hws_fileuuid)
        clientuuid = read (T.unpack hws_clientuuid) 
    (Gtk.postGUIAsync . evhandler . UsrEv) (GotSyncEvent fileuuid clientuuid) 
