{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Attoparsec
import           Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.ByteString.Char8 as B
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Traversable as Tr
import qualified Network.HTTP.Conduit as N
import           Network.Info
import           System.Environment
--
import Hoodle.Manage.Connect

data HoodleDevice = HoodleDevice { hoodleDeviceUuid  :: T.Text
                                 , hoodleDeviceOwner :: T.Text
                                 , hoodleDeviceIpaddr :: T.Text }
                  deriving (Show,Eq,Ord)


instance ToJSON HoodleDevice where 
  toJSON HoodleDevice {..} = object [ "uuid" .= toJSON hoodleDeviceUuid
                                    , "owner"  .= toJSON hoodleDeviceOwner
                                    , "ipaddr" .= toJSON hoodleDeviceIpaddr
                                    ] 

instance FromJSON HoodleDevice where 
  parseJSON (Object v) = HoodleDevice <$>
                         v .: "uuid"  <*>
                         v .: "owner" <*>
                         v .: "ipaddr"
  parseJSON _ = mzero



main :: IO ()
main = do 
  args <- getArgs
  mip <- getip (args !! 0) (args !! 1) (args !! 2)
  Tr.mapM TIO.putStr mip
  return ()
  --- F.mapM_ print mip

getip :: String -> String -> String -> IO (Maybe T.Text)
getip url idee pwd = do 
    hubwork url idee pwd $ \ck -> do 
        let uuid = "c3d45ece-b57a-4c61-9781-0ed7ebddf020"
        request'' <- N.parseUrl (url <> "/device/" <> uuid )

        let requesttask = request'' 
              { N.method = "GET"
              , N.requestHeaders = ("Content-Type", "application/json") 
                                   : ("Cookie",ck) 
                                   : N.requestHeaders request''
              }
        runResourceT $ N.withManager $ \manager -> do  
          response <- N.http requesttask manager
          content <- N.responseBody response $$+- CL.consume 

          -- liftIO $ print (mconcat content)
          case parseOnly json (mconcat content) of
            Left err -> error err
            Right v -> let mdev :: Maybe HoodleDevice = parseMaybe parseJSON v
                       in case mdev of 
                            Nothing -> error "parse error"
                            Just dev -> return (hoodleDeviceIpaddr dev)
                       -- liftIO $ print v
 --         liftIO $ B.putStrLn (mconcat content)
