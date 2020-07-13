{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, retry, writeTVar)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Loops (iterateM_)
import Control.Monad.Reader (ask)
import Control.Monad.State (put)
import Data.Acid (AcidState, Query, Update, makeAcidic, openLocalState, query, update)
import Data.Binary (encode)
import Data.Foldable (toList)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Sequence (Seq, ViewR ((:>)), (|>))
import qualified Data.Sequence as S (empty, filter, singleton, viewr)
import Data.Text (Text)
import Message
  ( C2SMsg (..),
    Commit (..),
    CommitId (..),
    S2CMsg (..),
    TextSerializable (deserialize, serialize),
    commitId,
  )
import qualified Network.Wai.Handler.Warp as Warp
import Network.WebSockets
  ( Connection,
    DataMessage (Binary),
    acceptRequest,
    receiveData,
    runServer,
    sendDataMessage,
    sendPing,
    sendTextData,
  )
import Servant ((:>), Get, JSON, Proxy (..), Server, serve)
import Type (Doc (..), Stroke (..))

getLast :: Seq a -> Maybe a
getLast s =
  case S.viewr s of
    _ :> x -> Just x
    _ -> Nothing

-- orphans.

$(deriveSafeCopy 0 'base ''CommitId)

$(deriveSafeCopy 0 'base ''Commit)

data DocState
  = DocState
      { _docStateCommits :: Seq Commit,
        _docStateCurrentDoc :: Seq (CommitId, Int, [(Double, Double)])
      }
  deriving (Show)

$(deriveSafeCopy 0 'base ''DocState)

writeState :: DocState -> Update DocState ()
writeState = put

queryState :: Query DocState DocState
queryState = ask

$(makeAcidic ''DocState ['writeState, 'queryState])

handler ::
  Connection ->
  AcidState DocState ->
  TVar DocState ->
  IO ()
handler conn acid ref = forever $ do
  t :: Text <- receiveData conn
  case deserialize t of
    NewStroke (hsh, coords) -> do
      s' <-
        atomically $ do
          DocState commits dat <- readTVar ref
          let (commits', dat') = case getLast commits of
                Just commitLast ->
                  let r = commitId commitLast
                      commit = Add (r + 1) coords
                   in (commits |> commit, dat |> (r + 1, hsh, coords))
                Nothing ->
                  let commit = Add 1 coords
                   in (S.singleton commit, S.singleton (1, hsh, coords))
              s' = DocState {_docStateCommits = commits', _docStateCurrentDoc = dat'}
          writeTVar ref s'
          pure s'
      update acid $ WriteState s'
    DeleteStrokes is -> do
      s' <-
        atomically $ do
          DocState commits dat <- readTVar ref
          let dat' = S.filter (\(i, _, _) -> not (i `elem` is)) dat
              commits' = case getLast commits of
                Just commitLast ->
                  let r = commitId commitLast
                      commit = Delete (r + 1) is
                   in commits |> commit
                Nothing -> S.singleton (Delete 1 is)
              s' = DocState {_docStateCommits = commits', _docStateCurrentDoc = dat'}
          writeTVar ref s'
          pure s'
      update acid $ WriteState s'
    SyncRequest (s, e) -> do
      if s == 0
        then do
          -- for initializing a client
          DocState _ currDoc <- atomically $ readTVar ref
          let commits = toList $ fmap (\(cid, _, strk) -> Add cid strk) currDoc
              msg = DataStrokes commits
          -- sendTextData conn (serialize msg)
          sendDataMessage conn (Binary (encode msg))
        else do
          -- for updating a already-initialized client
          DocState commits _ <- atomically $ readTVar ref
          let commits' =
                toList $
                  S.filter (\c -> let i = commitId c in i > s && i <= e) commits
              msg = DataStrokes commits'
          -- sendTextData conn (serialize msg)
          sendDataMessage conn (Binary (encode msg))

type API = "doc" :> Get '[JSON] Doc

api :: Proxy API
api = Proxy

server :: TVar DocState -> Server API
server var = do
  s <- liftIO $ atomically $ readTVar var
  let doc =
        Doc
          $ map (\(i, _, xys) -> Stroke i xys)
          $ toList (_docStateCurrentDoc s)
  pure doc

second :: Int
second = 1000000

pingPeriod :: Int
pingPeriod = 1 * second

-- | ping-pong every pingPeriod.
ping :: Connection -> IO ()
ping conn = forever $ do
  threadDelay pingPeriod
  sendPing conn ("ping" :: Text)

main :: IO ()
main = do
  acid <- openLocalState (DocState S.empty S.empty)
  s <- query acid QueryState
  ref <- newTVarIO s
  void $ forkIO $ runServer "192.168.1.42" 7080 $ \pending -> do
    conn <- acceptRequest pending
    putStrLn "websocket connected"
    void $ forkIO $ ping conn
    -- synchronization
    void $ forkIO $ handler conn acid ref
    void $ flip iterateM_ 0 $ \r -> do
      r' <-
        atomically $ do
          DocState commits _ <- readTVar ref
          case getLast commits of
            Just commit -> let r' = commitId commit in if (r' <= r) then retry else pure r'
            Nothing -> retry
      let msg = RegisterStroke r'
      sendTextData conn (serialize msg)
      pure r'
  Warp.run 7070 $ serve api (server ref)
