{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Type.Synchronization
-- Copyright   : (c) 2014, 2015 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Type.Synchronization where

import           Control.Applicative
import           Data.Aeson as AE
import           Data.Data
import qualified Data.HashMap.Strict as H
import           Data.Text
import           Data.Time.Clock (UTCTime)
import           Database.Persist.Sqlite
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
FileSyncStatus
    uuid    Text
    md5     Text
    time    UTCTime
    UniqueFileSyncStatusUUID uuid
    deriving Typeable
    deriving Show
|]


instance ToJSON FileSyncStatus where
  toJSON FileSyncStatus {..} = object [ "uuid" .= toJSON fileSyncStatusUuid 
                                      , "md5"  .= toJSON fileSyncStatusMd5
                                      , "time" .= toJSON fileSyncStatusTime 
                                      ]
instance FromJSON FileSyncStatus where
  parseJSON (Object v) = 
    let r = do
          String uuid   <- H.lookup "uuid" v
          String md5txt <- H.lookup "md5" v
          eutime <- fromJSON <$> H.lookup "time" v
          utime <- case eutime of
                     Error _ -> Nothing
                     Success a -> a
          return (FileSyncStatus uuid md5txt utime)
    in maybe (fail "error in parsing FileSyncStatus") return r
  parseJSON _ = fail "error in parsing FileSyncStatus" 


data FileContent = FileContent { file_uuid :: Text
                               , file_path :: Text
                               , file_content :: Text 
                               , file_rsync :: Maybe FileRsync
                               }
                 deriving Show

instance ToJSON FileContent where
    toJSON FileContent {..} = object [ "uuid"    .= toJSON file_uuid
                                     , "path"    .= toJSON file_path
                                     , "content" .= toJSON file_content 
                                     , "rsync"   .= toJSON file_rsync
                                     ]

data FileRsync = FileRsync { frsync_uuid :: Text 
                           , frsync_sig :: Text
                           }
               deriving Show

instance ToJSON FileRsync where
  toJSON FileRsync {..} = object [ "uuid" .= toJSON frsync_uuid
                                 , "signature" .= toJSON frsync_sig ]

instance FromJSON FileRsync where
  parseJSON (Object v) = 
    let r = do 
          String uuid <- H.lookup "uuid" v
          String sig <- H.lookup "signature" v
          return (FileRsync uuid sig)
    in maybe (fail "error in parsing FileRsync") return r
  parseJSON _ = fail "error in parsing FileRsync"
