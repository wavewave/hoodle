{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- common message type between server and client

module Message where

import Data.Text (Text)
import qualified Data.Text as T

data C2SMsg
  = -- | (hash,data)
    NewStroke (Int, [(Double, Double)])
  | -- | ids
    DeleteStrokes [Int]
  | -- | (id0,id1)
    SyncRequest (Int, Int)

tag_NewStroke :: Char
tag_NewStroke = 'N'

tag_DeleteStroke :: Char
tag_DeleteStroke = 'D'

tag_SyncRequest :: Char
tag_SyncRequest = 'S'

data Commit
  = Add Int [(Double, Double)]
  | Delete Int [Int]
  deriving (Show)

commitId :: Commit -> Int
commitId (Add i _) = i
commitId (Delete i _) = i

tag_Add :: Char
tag_Add = 'A'

tag_Delete :: Char
tag_Delete = 'D'

data S2CMsg
  = -- | id
    RegisterStroke Int
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
  serialize (DeleteStrokes payload) = T.cons 'D' $ T.pack (show payload)
  serialize (SyncRequest range) = T.cons 'S' $ T.pack (show range)

  deserialize t =
    let c = T.head t
     in case c of
          'N' -> NewStroke $ read $ T.unpack $ T.tail t
          'D' -> DeleteStrokes $ read $ T.unpack $ T.tail t
          'S' -> SyncRequest $ read $ T.unpack $ T.tail t
          _ -> error "cannot parse"

instance TextSerializable S2CMsg where
  serialize (RegisterStroke payload) = T.cons 'R' $ T.pack (show payload)
  serialize (DataStrokes commits) =
    T.cons 'D'
      $ T.intercalate ":"
      $ map
        ( \case
            Add i dat -> T.cons 'A' $ T.pack (show (i, dat))
            Delete i js -> T.cons 'D' $ T.pack (show (i, js))
        )
        commits

  deserialize t =
    let c = T.head t
     in case c of
          'R' -> RegisterStroke $ read $ T.unpack $ T.tail t
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
                                 in Add i dat
                              'D' ->
                                let (i, js) = read $ T.unpack $ T.tail tt
                                 in Delete i js
                    )
                    ts
             in DataStrokes commits
          _ -> error "cannot parse"
