{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Hoodle.Manage.Type.MD5 where

-- import Crypto.Hash.MD5
import qualified Data.ByteString as B
import Data.ByteString.Base16 as B16
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
-- import Data.Digest.Pure.MD5
import Database.Persist
import Database.Persist.Sql

newtype MD5 = MD5 { unMD5 :: B.ByteString } deriving Show

instance PersistField MD5 where
  toPersistValue :: MD5 -> PersistValue
  toPersistValue = PersistText . decodeUtf8 . B16.encode . unMD5

  fromPersistValue :: PersistValue -> Either T.Text MD5
  fromPersistValue (PersistText s) = 
    let (decoded,remained) = (B16.decode . encodeUtf8) s
    in if B.null remained 
         then (Right (MD5 decoded))
         else Left "Not a MD5"  
  fromPersistValue _ = Left "Not a text persist type"

instance PersistFieldSql MD5 where
  sqlType :: Monad m => m MD5 -> SqlType 
  sqlType _ = SqlString


