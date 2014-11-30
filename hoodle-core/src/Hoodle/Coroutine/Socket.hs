{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Socket
-- Copyright   : (c) 2014 Ian-Woo Kim
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
import           Control.Monad (unless)
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
-- import           Control.Monad.Trans.State
import           Data.Aeson as AE
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.CaseInsensitive as CI
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import Data.Monoid ((<>))
import Data.Text (Text,pack,unpack)
import Data.Text.Encoding (encodeUtf8,decodeUtf8)
import Data.Time.Calendar
import Data.Time.Clock
import Data.UUID.V4
import qualified Graphics.UI.Gtk as Gtk
import Network
import Network.Google.OAuth2 (formUrl, exchangeCode, refreshTokens,
                               OAuth2Client(..), OAuth2Tokens(..))
import Network.Google (makeRequest, doRequest)
import Network.HTTP.Client.Internal (computeCookieString)
import Network.HTTP.Conduit
import Network.HTTP.Types (methodPut)
import qualified Network.WebSockets as WS
import System.Directory
import System.Environment (getEnv)
import System.Exit    (ExitCode(..))
import System.FilePath ((</>),(<.>),makeRelative)
import System.Info (os)
import System.Process (system, rawSystem,readProcessWithExitCode)
--
-- import Data.Hoodle.Generic
import Data.Hoodle.Simple
import Graphics.Hoodle.Render.Type.Hoodle
import Text.Hoodle.Builder (builder)
--
import Hoodle.Coroutine.Dialog
import Hoodle.Script.Hook
import Hoodle.Type.Coroutine
import Hoodle.Type.Event
import Hoodle.Type.Hub
import Hoodle.Type.HoodleState
import Hoodle.Util

data Message = Message { msgbody :: Text } deriving (Show,Eq,Ord)

-- |
socketConnect :: MainCoroutine ()
socketConnect = do
    xst <- get
    uhdl <- view (unitHoodles.currentUnit) <$> get
    r <- runMaybeT $ do 
      hset <- (MaybeT . return) $ view hookSet xst
      hinfo <- (MaybeT . return) (hubInfo hset)
      -- let hdir = hubfileroot hinfo
      -- fp <- (MaybeT . return) (view (hoodleFileControl.hoodleFileName) uhdl)
      -- canfp <- liftIO $ canonicalizePath fp
      -- let relfp = makeRelative hdir canfp 

      -- liftIO $ print (hinfo,relfp)
      lift (socketWork hinfo)
    case r of 
      Nothing -> okMessageBox "socket connect not successful" >> return ()
      Just _ -> return ()  


socketWork :: HubInfo -> MainCoroutine ()
socketWork hinfo@(HubInfo {..}) = do
    hdl <- rHoodle2Hoodle . getHoodle . view (unitHoodles.currentUnit) <$> get
    hdir <- liftIO $ getHomeDirectory
    let tokfile = hdir </> ".hoodle.d" </> "token.txt"
        client = OAuth2Client { clientId = unpack cid, clientSecret = unpack secret }
        permissionUrl = formUrl client ["email"]
    liftIO (doesFileExist tokfile) >>= \b -> unless b $ do       
      liftIO $ putStrLn$ "Load this URL: "++show permissionUrl
      case os of
        "linux"  -> liftIO $ rawSystem "chromium" [permissionUrl]
        "darwin" -> liftIO $ rawSystem "open"       [permissionUrl]
        _        -> return ExitSuccess
      mauthcode <- textInputDialog "Please paste the verification code: "
      F.forM_ mauthcode $ \authcode -> do
        tokens   <- liftIO $ exchangeCode client authcode
        liftIO $ putStrLn$ "Received access token: "++show (accessToken tokens)
        liftIO $ writeFile tokfile (show tokens)
    doIOaction $ \evhandler -> do 
      forkIO $ (`E.catch` (\(err:: E.SomeException)-> print err >> return ())) $ 
        withSocketsDo $ withManager $ \manager -> do
          -- refresh token
          oldtok <- liftIO $ read <$> (readFile tokfile)

          newtok  <- liftIO $ refreshTokens client oldtok
          liftIO $ putStrLn$ "As a test, refreshed token: "++show (accessToken newtok)
          liftIO $ writeFile tokfile (show newtok)
          --
          accessTok <- fmap (accessToken . read) (liftIO (readFile tokfile))
          request' <- parseUrl authgoogleurl 
          let request = request' 
                { requestHeaders =  [ ("Authorization", encodeUtf8 $ "Bearer " <> pack accessTok) ]
                , cookieJar = Just (createCookieJar  [])
                }
          response <- httpLbs request manager
          let coojar = responseCookieJar response
          liftIO $ print coojar



          let uuidtxt = decodeUtf8 (view hoodleID hdl)
          request2' <- parseUrl ("http://" <> hubsocketurl <> ":" <> show hubsocketport </> hubsocketpath)

          ctime <- liftIO getCurrentTime
          let (bstr,_) = computeCookieString request2' coojar ctime True
              newheaders = [(CI.mk "Cookie",bstr)] 

          liftIO $ WS.runClientWith hubsocketurl hubsocketport hubsocketpath WS.defaultConnectionOptions newheaders $ \conn -> forever $ do 
            putStrLn "connected"
            Message txt <- WS.receiveData conn
            
            let urlpath = FileUrl (hubfileroot </> unpack txt) 
            (Gtk.postGUIAsync . evhandler . UsrEv) (OpenLink urlpath Nothing)

      return (UsrEv ActionOrdered)



