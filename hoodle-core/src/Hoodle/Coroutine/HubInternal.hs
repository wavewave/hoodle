{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.HubInternal
-- Copyright   : (c) 2014, 2015 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.HubInternal where

import           Control.Applicative
import           Control.Concurrent
import qualified Control.Exception as E
import           Control.Lens (view)
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Aeson as AE
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as H
import           Data.IORef
import Data.Monoid ((<>))
import Data.Text (Text,pack,unpack)
import Data.Text.Encoding (encodeUtf8,decodeUtf8)
import Data.UUID.V4
import qualified Graphics.UI.Gtk as Gtk
import Network
import Network.Google.OAuth2 (formUrl, exchangeCode, refreshTokens,
                               OAuth2Client(..), OAuth2Tokens(..))
import Network.HTTP.Client (GivesPopper)
import Network.HTTP.Conduit
import Network.HTTP.Types (methodPut)
import System.Directory
import System.Exit    (ExitCode(..))
import System.FilePath ((</>),(<.>))
import System.Info (os)
import System.Process (rawSystem,readProcessWithExitCode)
--
import Data.Hoodle.Simple
import Graphics.Hoodle.Render.Type.Hoodle
import Text.Hoodle.Builder (builder)
--
import Hoodle.Coroutine.Dialog
import Hoodle.Type.Coroutine
import Hoodle.Type.Event
import Hoodle.Type.Hub
import Hoodle.Type.HoodleState
import Hoodle.Type.Synchronization
--



streamContent :: BL.ByteString -> GivesPopper ()
streamContent lb np = do
    lbref <- newIORef lb 
    np (popper lbref)
  where popper lbref = do
          lbstr <- readIORef lbref
          if (not .BL.null) lbstr
            then do 
              let (lbstr1,lbstr2) = BL.splitAt 10240 lbstr
              writeIORef lbref lbstr2
              return (BL.toStrict lbstr1)
            else do
              return ""
         
uploadWork :: (FilePath,FilePath) -> HubInfo -> MainCoroutine ()
uploadWork (ofilepath,filepath) hinfo@(HubInfo {..}) = do
    hdl <- rHoodle2Hoodle . getHoodle . view (unitHoodles.currentUnit) <$> get
    hdir <- liftIO $ getHomeDirectory
    let tokfile = hdir </> ".hoodle.d" </> "token.txt"
        client = OAuth2Client { clientId = unpack cid, clientSecret = unpack secret }
        permissionUrl = formUrl client ["email"]
    liftIO (doesFileExist tokfile) >>= \b -> unless b $ do       
      case os of
        "linux"  -> liftIO $ rawSystem "chromium" [permissionUrl]
        "darwin" -> liftIO $ rawSystem "open"       [permissionUrl]
        _        -> return ExitSuccess
      mauthcode <- textInputDialog "Please paste the verification code: "
      F.forM_ mauthcode $ \authcode -> do
        tokens   <- liftIO $ exchangeCode client authcode
        liftIO $ writeFile tokfile (show tokens)
    doIOaction $ \evhandler -> do 
      forkIO $ (`E.catch` (\(_ :: E.SomeException)-> (Gtk.postGUIAsync . evhandler . UsrEv) (DisconnectedHub tokfile (ofilepath,filepath) hinfo) >> return ())) $ 
        withSocketsDo $ withManager $ \manager -> do
          -- refresh token
          oldtok <- liftIO $ read <$> (readFile tokfile)

          newtok  <- liftIO $ refreshTokens client oldtok
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
          let uuidtxt = decodeUtf8 (view hoodleID hdl)
          request2' <- parseUrl (hubfileurl </> unpack uuidtxt )
          let request2 = request2' 
                { requestHeaders = [ ("Accept", "application/json; charset=utf-8") ] 
                , cookieJar = Just coojar }
          response2 <- httpLbs request2 manager
          let mfrsync = AE.decode (responseBody response2) :: Maybe FileRsync
              hdlbstr = (BL.toStrict . builder) hdl
          b64txt <- case mfrsync of 
            Nothing -> (return . decodeUtf8 . B64.encode) hdlbstr
            Just frsync -> liftIO $ do
              let rsyncbstr = (B64.decodeLenient . encodeUtf8 . frsync_sig) frsync
              tdir <- getTemporaryDirectory
              uuid'' <- nextRandom
              let tsigfile = tdir </> show uuid'' <.> "sig"
                  tdeltafile = tdir </> show uuid'' <.> "delta"
              B.writeFile tsigfile rsyncbstr
              readProcessWithExitCode "rdiff" 
                ["delta", tsigfile, ofilepath, tdeltafile] ""
              deltabstr <- B.readFile tdeltafile 
              mapM_ removeFile [tsigfile,tdeltafile]
              (return . decodeUtf8 . B64.encode) deltabstr
          let filecontent = toJSON FileContent { file_uuid = uuidtxt
                                               , file_path = pack filepath
                                               , file_content = b64txt 
                                               , file_rsync = mfrsync }
              filecontentbstr = encode filecontent
          request3' <- parseUrl (hubfileurl </> unpack uuidtxt )
          let request3 = request3' { method = methodPut
                                   , requestBody = RequestBodyStreamChunked (streamContent filecontentbstr)
                                   , cookieJar = Just coojar }
          _response3 <- httpLbs request3 manager
          return ()
      return (UsrEv ActionOrdered)


