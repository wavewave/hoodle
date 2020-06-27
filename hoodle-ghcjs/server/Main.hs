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
import Control.Monad.IO.Class (liftIO)
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
    acceptRequest,
    receiveData,
    runServer,
    sendPing,
    sendTextData,
  )
import Servant ((:>), Get, JSON, Proxy (..), Server, serve)
import System.IO (IOMode (..), hFlush, hPutStrLn, withFile)
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
      DocState commits _ <- atomically $ readTVar ref
      let commits' =
            toList $
              S.filter (\c -> let i = commitId c in i > s && i <= e) commits
          msg = DataStrokes commits'
      sendTextData conn (serialize msg)

serializer :: AcidState DocState -> IO ()
serializer acid = forever $ do
  threadDelay 5000000
  DocState _ v <- query acid QueryState
  withFile "serialized.dat" WriteMode $ \h -> do
    hPutStrLn h (show v)
    hFlush h

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
