{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hoodle.Coroutine.Hub.Common where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Data.Aeson as AE
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Foldable as F
import Data.IORef
import Data.Monoid ((<>))
import qualified Data.Text as T (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Database.Persist (entityVal, getBy)
import Database.Persist.Sqlite (runSqlite)
--
--
import Hoodle.Coroutine.Dialog
import Hoodle.Type.Coroutine
import Hoodle.Type.Hub
import Hoodle.Type.Synchronization
import Network
import Network.Google.OAuth2
  ( OAuth2Client (..),
    OAuth2Tokens (..),
    exchangeCode,
    formUrl,
    refreshTokens,
  )
import Network.HTTP.Client (GivesPopper)
import Network.HTTP.Conduit
  ( CookieJar,
    Manager,
    cookieJar,
    createCookieJar,
    httpLbs,
    parseUrl,
    requestHeaders,
    responseBody,
    responseCookieJar,
    withManager,
  )
import System.Directory
import System.Exit (ExitCode (..))
import System.Info (os)
import System.Process (rawSystem)

--

-- |
streamContent :: BL.ByteString -> GivesPopper ()
streamContent lb np = do
  lbref <- newIORef lb
  np (popper lbref)
  where
    popper lbref = do
      lbstr <- readIORef lbref
      if (not . BL.null) lbstr
        then do
          let (lbstr1, lbstr2) = BL.splitAt 10240 lbstr
          writeIORef lbref lbstr2
          return (BL.toStrict lbstr1)
        else do
          return ""

-- |
prepareToken :: HubInfo -> FilePath -> MainCoroutine ()
prepareToken HubInfo {..} tokfile = do
  let client = OAuth2Client {clientId = T.unpack googleClientId, clientSecret = T.unpack googleClientSecret}
      permissionUrl = formUrl client ["email"]
  liftIO $ putStrLn "prepareToken"
  liftIO (doesFileExist tokfile) >>= \b -> unless b $ do
    case os of
      "linux" -> liftIO $ rawSystem "chromium" [permissionUrl]
      "darwin" -> liftIO $ rawSystem "open" [permissionUrl]
      _ -> return ExitSuccess
    mauthcode <- textInputDialog "Please paste the verification code: "
    F.forM_ mauthcode $ \authcode -> do
      tokens <- liftIO $ exchangeCode client authcode
      liftIO $ writeFile tokfile (show tokens)

-- |
withHub ::
  HubInfo ->
  FilePath ->
  (Manager -> CookieJar -> ResourceT IO a) ->
  IO a
withHub HubInfo {..} tokfile action =
  withSocketsDo $ withManager $ \manager -> do
    let client =
          OAuth2Client
            { clientId = T.unpack googleClientId,
              clientSecret = T.unpack googleClientSecret
            }
    -- refresh token
    oldtok <- liftIO $ read <$> (readFile tokfile)
    newtok <- liftIO $ refreshTokens client oldtok
    liftIO $ writeFile tokfile (show newtok)
    --
    accessTok <- fmap (accessToken . read) (liftIO (readFile tokfile))
    request' <- parseUrl googleAuthURL
    let request =
          request'
            { requestHeaders = [("Authorization", encodeUtf8 $ "Bearer " <> T.pack accessTok)],
              cookieJar = Just (createCookieJar [])
            }
    response <- httpLbs request manager
    let coojar = responseCookieJar response
    action manager coojar

sessionGetJSON ::
  (FromJSON a) =>
  String ->
  ReaderT (Manager, CookieJar) (ResourceT IO) (Maybe a)
sessionGetJSON url = do
  (manager, coojar) <- ask
  req' <- lift $ parseUrl url
  let req =
        req'
          { requestHeaders = [("Accept", "application/json; charset=utf-8")],
            cookieJar = Just coojar
          }
  res <- lift $ httpLbs req manager
  return (AE.decode (responseBody res))

-- |
getLastSyncStatus :: FilePath -> T.Text -> IO (Maybe FileSyncStatus)
getLastSyncStatus fp uuidtxt =
  fmap entityVal <$> runSqlite (T.pack fp) (getBy (UniqueFileSyncStatusUUID uuidtxt))
