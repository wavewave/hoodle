{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Hoodle.Manage.Type.UUID 
( module Data.UUID
, module Data.UUID.V4
) where

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.UUID
import Data.UUID.V4
import Database.Persist
import Database.Persist.Sql

instance PersistField UUID where
  toPersistValue :: UUID -> PersistValue
  toPersistValue = PersistText . decodeUtf8 . toASCIIBytes

  fromPersistValue :: PersistValue -> Either Text UUID
  fromPersistValue (PersistText s) = case (fromASCIIBytes . encodeUtf8) s of
                                       Nothing -> Left s 
                                       Just uuid -> Right uuid
  fromPersistValue _ = Left "Not a text persist type"

instance PersistFieldSql UUID where
  sqlType :: Monad m => m UUID -> SqlType 
  sqlType _ = SqlString

