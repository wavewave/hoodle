{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, retry, writeTVar)
import Control.Monad (forever, void)
import Control.Monad.Loops (iterateM_)
import Control.Monad.Reader (ask)
import Control.Monad.State (put)
import Data.Acid (AcidState, Query, Update, makeAcidic, openLocalState, query, update)
import Data.Foldable (toList)
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)
import Data.Sequence (Seq, ViewR ((:>)), (|>))
import qualified Data.Sequence as S (empty, filter, length, singleton, viewr)
import Data.Text (Text)
import qualified Data.Text as T
import Message (C2SMsg (..), S2CMsg (..), TextSerializable (deserialize, serialize))
import qualified Network.Wai.Handler.Warp as Warp
import Network.WebSockets (Connection, acceptRequest, receiveData, runServer, sendPing, sendTextData)
import Servant ((:>), Get, JSON, Proxy (..), Server, serve)
import System.IO (IOMode (..), hFlush, hPutStrLn, withFile)

type API = "hello" :> Get '[JSON] String

api :: Proxy API
api = Proxy

server :: Server API
server = pure "hello world"

getLast :: Seq a -> Maybe a
getLast s =
  case S.viewr s of
    _ :> x -> Just x
    _ -> Nothing

data Commit
  = Add Int [(Double, Double)]
  | Delete [Int]
  deriving (Show)

$(deriveSafeCopy 0 'base ''Commit)

data DocState
  = DocState
      { _docStateCommits :: Seq (Int, Commit),
        _docStateCurrentDoc :: Seq (Int, Int, [(Double, Double)])
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
          let commit = Add hsh coords
              (commits', dat') = case getLast commits of
                Just (r, _) ->
                  (commits |> (r + 1, commit), dat |> (r + 1, hsh, coords))
                Nothing ->
                  (S.singleton (1, commit), S.singleton (1, hsh, coords))
              s' = DocState {_docStateCommits = commits', _docStateCurrentDoc = dat'}
          writeTVar ref s'
          pure s'
      update acid $ WriteState s'
    DeleteStrokes is -> do
      s' <-
        atomically $ do
          DocState commits dat <- readTVar ref
          let commit = Delete is
              dat' = S.filter (\(i, _, _) -> not (i `elem` is)) dat
              commits' = case getLast commits of
                Just (r, _) -> commits |> (r + 1, commit)
                Nothing -> S.singleton (1, commit)
              s' = DocState {_docStateCommits = commits', _docStateCurrentDoc = dat'}
          writeTVar ref s'
          pure s'
      print $ S.length (_docStateCurrentDoc s')
      update acid $ WriteState s'
    SyncRequest (s, e) -> do
      DocState _ dat <- atomically $ readTVar ref
      let dat' =
            toList
              $ fmap (\(i, _, xy) -> (i, xy))
              $ S.filter (\(i, _, _) -> i > s && i <= e) dat
          msg = DataStrokes dat'
      sendTextData conn (serialize msg)

serializer :: AcidState DocState -> IO ()
serializer acid = forever $ do
  threadDelay 5000000
  DocState _ v <- query acid QueryState
  withFile "serialized.dat" WriteMode $ \h -> do
    hPutStrLn h (show v)
    hFlush h

main :: IO ()
main = do
  acid <- openLocalState (DocState S.empty S.empty)
  s <- query acid QueryState
  ref <- newTVarIO s
  void $ forkIO $ runServer "192.168.1.42" 7080 $ \pending -> do
    conn <- acceptRequest pending
    putStrLn "websocket connected"
    -- ping-pong every second
    void $ forkIO $ forever $ do
      threadDelay 1000000
      sendPing conn ("ping" :: Text)
    -- synchronization
    void $ forkIO $ handler conn acid ref
    void $ forkIO $ serializer acid
    void $ flip iterateM_ 0 $ \r -> do
      (r', hsh') <-
        atomically $ do
          DocState _ dat <- readTVar ref
          case getLast dat of
            Just (r', hsh', _) -> if (r' <= r) then retry else pure (r', hsh')
            Nothing -> retry
      let msg = RegisterStroke (r', hsh')
      sendTextData conn (serialize msg)
      pure r'
  Warp.run 7070 $ serve api server
