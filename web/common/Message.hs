{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- common message type between server and client

module Message where

import Data.Binary (Binary (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)

newtype CommitId = CommitId {unCommitId :: Int}
  deriving (Show, Eq, Ord, Num, Binary)

data C2SMsg
  = -- | (hash,data)
    NewStroke (Int, [(Double, Double)])
  | -- | ids
    DeleteStrokes [CommitId]
  | -- | (id0,id1)
    SyncRequest (CommitId, CommitId)

instance Binary C2SMsg where
  put (NewStroke hshData) = do
    put (0 :: Word8)
    put hshData
  put (DeleteStrokes is) = do
    put (1 :: Word8)
    put is
  put (SyncRequest startend) = do
    put (2 :: Word8)
    put startend

  get = do
    t :: Word8 <- get
    case t of
      0 -> NewStroke <$> get
      1 -> DeleteStrokes <$> get
      2 -> SyncRequest <$> get

data Commit
  = Add CommitId [(Double, Double)]
  | Delete CommitId [CommitId]
  deriving (Show)

instance Binary Commit where
  put (Add i strk) = do
    put (0 :: Word8)
    put i
    put strk
  put (Delete i js) = do
    put (1 :: Word8)
    put i
    put js

  get = do
    t :: Word8 <- get
    case t of
      0 -> Add <$> get <*> get
      1 -> Delete <$> get <*> get

commitId :: Commit -> CommitId
commitId (Add i _) = i
commitId (Delete i _) = i

data S2CMsg
  = -- | id
    RegisterStroke CommitId
  | -- | commits
    DataStrokes [Commit]

instance Binary S2CMsg where
  put (RegisterStroke i) = put (0 :: Word8) >> put i
  put (DataStrokes cs) = put (1 :: Word8) >> put cs

  get = do
    t :: Word8 <- get
    case t of
      0 -> RegisterStroke <$> get
      1 -> DataStrokes <$> get
