{-# LANGUAGE GADTs, OverloadedStrings #-}

module Control.Monad.Coroutine.Logger.WebLog where

import           Control.Monad.Reader 
import           Data.Aeson.Types 
import           Data.Aeson.Encode as E
import           Data.Aeson.Parser
import qualified Data.Attoparsec as A
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Char8 as SC
import           Network.HTTP.Types
import           Network.HTTP.Types.Status
import           Network.HTTP.Conduit
import           System.FilePath
-- from other hep-platform package
import           Application.WebLogger.Type 
-- from this package
import           Control.Monad.Coroutine
import           Control.Monad.Coroutine.Logger 
import           Control.Monad.Coroutine.Object

type Url = String 

-- | 
weblogger :: (MonadIO m) => Url -> LogServer m () 
weblogger url = webloggerW url 0
 

-- |
webloggerW :: (MonadIO m) => Url -> Int -> LogServer m () 
webloggerW url num = ReaderT (f num)
  where 
    f n req = 
      case req of 
        Input WriteLog msg -> do 
          let logmsg = WebLoggerInfo ("log number " ++ show n ++ " : " ++ msg)
          liftIO $ comm url "upload" methodPost logmsg 
          req' <- request (Output WriteLog ())
          f (n+1) req' 

-- | 
comm :: Url 
     -> String 
     -> Method 
     -> WebLoggerInfo 
     -> IO (Either String (Result Value))
comm url api mthd mi = do 
  request <- parseUrl (url </> api)
  withManager $ \manager -> do
    let mijson = E.encode (toJSON mi)
        myrequestbody = RequestBodyLBS mijson 
    let requestjson = request 
          { method = mthd
          , requestHeaders = [ ("Accept", "application/json; charset=utf-8") ]
          , requestBody = myrequestbody } 
    r <- httpLbs requestjson manager 
    if statusCode (responseStatus r) == 200 
      then return . parseJson . SC.concat . C.toChunks . responseBody $ r
      else return (Left $ "status code : " ++ show (statusCode (responseStatus r))) 

-- | 
parseJson :: (FromJSON a) => SC.ByteString -> Either String (Result a)
parseJson bs =
  let resultjson = A.parse json bs 
  in case resultjson of 
       (A.Done rest rjson) -> return (parse parseJSON rjson)
       _                 -> Left "parseJson" 
