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
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List as L
import Data.Monoid ((<>))
import Data.Text (Text,pack,unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Calendar
import Data.Time.Clock
import Network
import Network.Google.OAuth2 (formUrl, exchangeCode, refreshTokens,
                               OAuth2Client(..), OAuth2Tokens(..))
import Network.Google (makeRequest, doRequest)
import Network.HTTP.Conduit
import System.Directory (doesFileExist,getHomeDirectory)
import System.FilePath ((</>))
import System.Exit    (ExitCode(..))
import System.Info (os)
import System.Process (system, rawSystem,readProcess)
--
import Hoodle.Script.Hook
import Hoodle.Type.Coroutine
import Hoodle.Type.Hub
import Hoodle.Type.HoodleState

hubtestCoroutine :: MainCoroutine ()
hubtestCoroutine = do
    xst <- get
    runMaybeT $ do 
      hset <- (MaybeT . return) (view hookSet xst)
      hinfo <- (MaybeT . return) (hubInfo hset)
      liftIO (hubtest hinfo)  
    return ()

hubtest :: HubInfo -> IO ()
hubtest HubInfo {..} = do
    hdir <- getHomeDirectory
    let file = hdir </> ".hoodle.d" </> "token.txt"
    withSocketsDo $ withManager $ \manager -> do
      -- Ask for permission to read/write your fusion tables:
      let client = OAuth2Client { clientId = unpack cid, clientSecret = unpack secret }
          permissionUrl = formUrl client ["email"]
      liftIO $ doesFileExist file >>= \b -> unless b $ do       
        putStrLn$ "Load this URL: "++show permissionUrl
        case os of
          "linux"  -> rawSystem "chromium" [permissionUrl]
          "darwin" -> rawSystem "open"       [permissionUrl]
          _        -> return ExitSuccess
        putStrLn "Please paste the verification code: "
        authcode <- getLine
        tokens   <- exchangeCode client authcode
        putStrLn$ "Received access token: "++show (accessToken tokens)
        tokens2  <- refreshTokens client tokens
        putStrLn$ "As a test, refreshed token: "++show (accessToken tokens2)
        writeFile file (show tokens2)

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

      request2' <- liftIO $ parseUrl huburl
      let request2 = request2' { cookieJar = Just coojar }
      response2 <- httpLbs request2 manager
      liftIO $ print response2



