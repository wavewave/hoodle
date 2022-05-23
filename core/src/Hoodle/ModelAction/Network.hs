{-# LANGUAGE OverloadedStrings #-}

-- THIS MODULE WILL BE REMOVED AFTER REVIEW.

module Hoodle.ModelAction.Network where

{-
import Control.Monad
import Data.Aeson.Encode as E
import Data.Aeson.Parser
import Data.Aeson.Types
import qualified Data.Attoparsec as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as SC
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Time.Clock
import Data.UUID
import Data.UUID.V5
import Data.Xournal.BBox
import Data.Xournal.Generic
import Data.Xournal.Simple
import Debug.Trace
import Hoodle.NetworkClipboard.Client.Config
import Hoodle.NetworkClipboard.Type
import Hoodle.Type.Clipboard
import Network.HTTP.Enumerator
import Network.HTTP.Types hiding (statusCode)
import System.Directory
import System.FilePath
import Unsafe.Coerce
-}

{-
type Url = String

nextUUID :: HoodleClipClientConfiguration -> IO UUID
nextUUID mc = do
  let c = hoodleclipClientURL mc
  t <- getCurrentTime
  return . generateNamed namespaceURL . B.unpack . SC.pack $ c ++ "/" ++ show t

startCreate :: HoodleClipClientConfiguration -> [Stroke] -> IO ()
startCreate mc strs = do
  putStrLn "job started"
  cwd <- getCurrentDirectory
  let url = hoodleclipServerURL mc
  uuid <- nextUUID mc
  let info =
        HoodleClipInfo
          { hoodleclip_uuid = uuid,
            hoodleclip_strokes = strs
          }
  response <- hoodleclipToServer url ("uploadhoodleclip") methodPost info
  -- putStrLn $ show response
  return ()

{-
  case A.parseOnly json jsonstr of
    Left str -> error str
    Right jsonstrokes ->
      case parse parseJSON jsonstrokes of
        Error str2 -> error str2
        Success strokes -> do
-}

startCurrent :: HoodleClipClientConfiguration -> IO (Maybe HoodleClipInfo)
startCurrent mc = do
  putStrLn $ "currentclip"
  let url = hoodleclipServerURL mc
  r <- jsonFromServer url ("currentclip") methodGet
  -- putStrLn $ show r
  case r of
    Right (Success v') ->
      case (parse parseJSON v' :: Result HoodleClipInfo) of
        Success hinfo -> return (Just hinfo)
        _ -> return Nothing
    _ -> return Nothing

startGet :: HoodleClipClientConfiguration -> String -> IO ()
startGet mc idee = do
  putStrLn $ "get " ++ idee
  let url = hoodleclipServerURL mc
  r <- jsonFromServer url ("hoodleclip" </> idee) methodGet
  -- putStrLn $ show r
  case r of
    Right v -> case v of
      Success v' -> putStrLn $ show (parse parseJSON v' :: Result HoodleClipInfo)
      _ -> return ()
    Left _ -> return ()

{-
startPut :: HoodleClipClientConfiguration
         -> String  -- ^ hoodleclip idee
         -> String  -- ^ hoodleclip name
         -> IO ()
startPut mc idee jsonstr = do
  putStrLn "job started"
  cwd <- getCurrentDirectory
  let url = hoodleclipServerURL mc
  case A.parseOnly json (SC.pack jsonstr) of
    Left str -> error str
    Right jsonstrokes ->
      case parse parseJSON jsonstrokes of
        Error str2 -> error str2
        Success strokes -> do
          let info = case fromString idee of
                       Nothing -> error "strange in startPut"
                       Just idee' -> HoodleClipInfo { hoodleclip_uuid = idee'
                                                      , hoodleclip_strokes = strokes }
          response <- hoodleclipToServer url ("hoodleclip" </> idee) methodPut info
          putStrLn $ show response
-}

{-
startDelete :: HoodleClipClientConfiguration -> String -> IO ()
startDelete mc idee = do
  putStrLn "job started"
  let url = hoodleclipServerURL mc
  r <- jsonFromServer url ("hoodleclip" </> idee) methodDelete
  putStrLn $ show r
-}

startGetList :: HoodleClipClientConfiguration -> IO ()
startGetList mc = do
  putStrLn "getlist: "
  let url = hoodleclipServerURL mc
  r <- jsonFromServer url ("listhoodleclip") methodGet
  putStrLn $ show r

jsonFromServer :: Url -> String -> Method -> IO (Either String (Result Value))
jsonFromServer url api mthd = do
  request <- parseUrl (url </> api)
  withManager $ \manager -> do
    let requestjson =
          request
            { method = mthd,
              requestHeaders = [("Accept", "application/json; charset=utf-8")]
            }
    r <- httpLbs requestjson manager
    if statusCode r == 200
      then return . parseJson . SC.concat . C.toChunks . responseBody $ r
      else return (Left $ "status code : " ++ show (statusCode r))

hoodleclipToServer :: Url -> String -> Method -> HoodleClipInfo -> IO (Either String (Result Value))
hoodleclipToServer url api mthd mi = do
  {-  let mijson = E.encode (toJSON mi)
        strict_mijson = C.toChunks mijson
    putStrLn $ show $ length strict_mijson
    return $ Left "test" -}
  -- Temporarily

  request <- parseUrl (url </> api)
  withManager $ \manager -> do
    let mijson = E.encode (toJSON mi)
        myrequestbody = RequestBodyLBS mijson
    let requestjson =
          request
            { method = mthd,
              requestHeaders = [("Accept", "application/json; charset=utf-8")],
              requestBody = myrequestbody
            }
    r <- httpLbs requestjson manager
    if statusCode r == 200
      then return . parseJson . SC.concat . C.toChunks . responseBody $ r
      else return (Left $ "status code : " ++ show (statusCode r))

parseJson :: (FromJSON a) => SC.ByteString -> Either String (Result a)
parseJson bs =
  let resultjson = trace (SC.unpack bs) $ A.parse json bs
   in case resultjson of
        (A.Done rest rjson) -> return (parse parseJSON rjson)
        _ -> Left "parseJson"

testHoodleClipClientConfiguration =
  HoodleClipClientConfiguration
    { hoodleclipServerURL = "http://localhost:7800",
      hoodleclipClientURL = "susy"
    }

copyContentsToNetworkClipboard :: HoodleClipClientConfiguration -> Clipboard -> IO ()
copyContentsToNetworkClipboard ncconf clip = do
  if not . isEmpty $ clip
    then do
      let strs = fmap gToStroke . getClipContents $ clip
      startCreate ncconf strs
    else putStrLn "no clipboard content"

getContentsFromNetworkClipboard :: HoodleClipClientConfiguration -> IO (Maybe Clipboard)
getContentsFromNetworkClipboard ncconf = do
  r <- startCurrent ncconf
  let mclip = fmap (Clipboard . fmap gFromStroke . hoodleclip_strokes) r
  return mclip
-}
