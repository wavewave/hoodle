{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- common message type between server and client

module Message where

import Data.Text (Text)
import qualified Data.Text as T

newtype CommitId = CommitId {unCommitId :: Int}
  deriving (Show, Eq, Ord, Num)

data C2SMsg
  = -- | (hash,data)
    NewStroke (Int, [(Double, Double)])
  | -- | ids
    DeleteStrokes [CommitId]
  | -- | (id0,id1)
    SyncRequest (CommitId, CommitId)

tag_NewStroke :: Char
tag_NewStroke = 'N'

tag_DeleteStroke :: Char
tag_DeleteStroke = 'D'

tag_SyncRequest :: Char
tag_SyncRequest = 'S'

data Commit
  = Add CommitId [(Double, Double)]
  | Delete CommitId [CommitId]
  deriving (Show)

commitId :: Commit -> CommitId
commitId (Add i _) = i
commitId (Delete i _) = i

tag_Add :: Char
tag_Add = 'A'

tag_Delete :: Char
tag_Delete = 'D'

data S2CMsg
  = -- | id
    RegisterStroke CommitId
  | -- | commits
    DataStrokes [Commit]

tag_RegisterStroke :: Char
tag_RegisterStroke = 'R'

tag_DataStrokes :: Char
tag_DataStrokes = 'D'

class TextSerializable a where
  serialize :: a -> Text
  deserialize :: Text -> a

instance TextSerializable C2SMsg where
  serialize (NewStroke payload) = T.cons 'N' $ T.pack (show payload)
  serialize (DeleteStrokes payload) = T.cons 'D' $ T.pack $ show $ map unCommitId payload
  serialize (SyncRequest (s, e)) = T.cons 'S' $ T.pack (show (unCommitId s, unCommitId e))

  deserialize t =
    let c = T.head t
     in case c of
          'N' -> NewStroke $ read $ T.unpack $ T.tail t
          'D' -> DeleteStrokes $ map CommitId $ read $ T.unpack $ T.tail t
          'S' ->
            SyncRequest $
              let (s, e) = read $ T.unpack $ T.tail t
               in (CommitId s, CommitId e)
          _ -> error "cannot parse"

instance TextSerializable S2CMsg where
  serialize (RegisterStroke commit) = T.cons 'R' $ T.pack $ show (unCommitId commit)
  serialize (DataStrokes commits) =
    T.cons 'D'
      $ T.intercalate ":"
      $ map
        ( \case
            Add (CommitId i) dat -> T.cons 'A' $ T.pack (show (i, dat))
            Delete (CommitId i) js -> T.cons 'D' $ T.pack (show (i, map unCommitId js))
        )
        commits

  deserialize t =
    let c = T.head t
     in case c of
          'R' -> RegisterStroke $ CommitId $ read $ T.unpack $ T.tail t
          'D' ->
            let t' = T.tail t
                ts = T.splitOn ":" t'
                commits =
                  map
                    ( \tt ->
                        let c' = T.head tt
                         in case c' of
                              'A' ->
                                let (i, dat) = read $ T.unpack $ T.tail tt
                                 in Add (CommitId i) dat
                              'D' ->
                                let (i, js) = read $ T.unpack $ T.tail tt
                                 in Delete (CommitId i) (map CommitId js)
                    )
                    ts
             in DataStrokes commits
          _ -> error "cannot parse"
