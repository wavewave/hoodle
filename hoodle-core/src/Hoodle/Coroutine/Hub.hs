{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Hub
-- Copyright   : (c) 2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Hub where

import           Control.Applicative
import qualified Control.Exception as E
import           Control.Lens (view)
import           Control.Monad (unless)
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
-- import           Control.Monad.Trans.State
import           Data.Aeson
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Foldable as F
import qualified Data.List as L
import Data.Monoid ((<>))
import Data.Text (Text,pack,unpack)
import Data.Text.Encoding (encodeUtf8,decodeUtf8)
import Data.Time.Calendar
import Data.Time.Clock
import Network
import Network.Google.OAuth2 (formUrl, exchangeCode, refreshTokens,
                               OAuth2Client(..), OAuth2Tokens(..))
import Network.Google (makeRequest, doRequest)
import Network.HTTP.Conduit
import Network.HTTP.Types (methodPut)
import System.Directory (doesFileExist,getHomeDirectory)
import System.FilePath ((</>))
import System.Exit    (ExitCode(..))
import System.Info (os)
import System.Process (system, rawSystem,readProcess)
--
-- import Data.Hoodle.Generic
import Data.Hoodle.Simple
import Graphics.Hoodle.Render.Type.Hoodle
import Text.Hoodle.Builder (builder)
--
import Hoodle.Coroutine.Dialog
import Hoodle.Script.Hook
import Hoodle.Type.Coroutine
import Hoodle.Type.Hub
import Hoodle.Type.HoodleState
import Hoodle.Util

data FileContent = FileContent { file_uuid :: Text, file_content :: Text }
                 deriving Show

instance ToJSON FileContent where
    toJSON FileContent {..} = object [ "uuid" .= toJSON file_uuid
                                     , "content" .= toJSON file_content ]


hubtestCoroutine :: MainCoroutine ()
hubtestCoroutine = do
    xst <- get
    runMaybeT_ $ do 
      hset <- (MaybeT . return) (view hookSet xst)
      hinfo <- (MaybeT . return) (hubInfo hset)
      lift (hubtest hinfo)

hubtest :: HubInfo -> MainCoroutine ()
hubtest HubInfo {..} = do
    hdl <- rHoodle2Hoodle . getHoodle . view (unitHoodles.currentUnit) <$> get
    hdir <- liftIO $ getHomeDirectory
    let file = hdir </> ".hoodle.d" </> "token.txt"
        client = OAuth2Client { clientId = unpack cid, clientSecret = unpack secret }
        permissionUrl = formUrl client ["email"]
    liftIO (doesFileExist file) >>= \b -> unless b $ do       
      liftIO $ putStrLn$ "Load this URL: "++show permissionUrl
      case os of
        "linux"  -> liftIO $ rawSystem "chromium" [permissionUrl]
        "darwin" -> liftIO $ rawSystem "open"       [permissionUrl]
        _        -> return ExitSuccess
      mauthcode <- textInputDialog "Please paste the verification code: "
      F.forM_ mauthcode $ \authcode -> do
        tokens   <- liftIO $ exchangeCode client authcode
        liftIO $ putStrLn$ "Received access token: "++show (accessToken tokens)
        -- Ask for permission to read/write your fusion tables:
        -- tokens2  <- liftIO $ refreshTokens client tokens
        -- putStrLn$ "As a test, refreshed token: "++show (accessToken tokens2)
        -- writeFile file (show tokens2)
        liftIO $ writeFile file (show tokens)

    liftIO . (`E.catch` (\(StatusCodeException _ _ _) -> return ())) $ withSocketsDo $ withManager $ \manager -> do
      accessTok <- fmap (accessToken . read) (liftIO (readFile file))
      request' <- liftIO $ parseUrl authgoogleurl 
      let request = request' 
            { requestHeaders =  [ ("Authorization", encodeUtf8 $ "Bearer " <> pack accessTok) ]
            , cookieJar = Just (createCookieJar  [])
            }
      response <- httpLbs request manager
      liftIO $ print response
      let coojar = responseCookieJar response

      liftIO $ print coojar

      let uuidtxt = decodeUtf8 (view hoodleID hdl)
          b64txt = (decodeUtf8 . B64.encode . BL.toStrict . builder) hdl

          filecontent = toJSON FileContent { file_uuid = uuidtxt, file_content = b64txt }


      request2' <- liftIO $ parseUrl (huburl </> unpack uuidtxt )
      let request2 = request2' { method = methodPut
                               , requestBody = RequestBodyLBS (encode filecontent)
                               , cookieJar = Just coojar }
      response2 <- httpLbs request2 manager
      liftIO $ print response2



