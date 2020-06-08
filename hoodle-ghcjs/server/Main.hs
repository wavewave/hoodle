{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Concurrent (forkIO,threadDelay)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, retry, writeTVar)
import Control.Monad (forever,void)
import Control.Monad.Loops (iterateM_)
import Data.Sequence (Seq,ViewR((:>)),(|>))
import qualified Data.Sequence as S (empty,viewr)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp
import Network.WebSockets ( acceptRequest, receiveData, runServer, sendPing, sendTextData )
import Servant ( (:>), Get, JSON, Proxy(..), Server, serve )

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

main :: IO ()
main = do
  putStrLn "servant server"
  ref <- newTVarIO S.empty

  void $ forkIO $ runServer "127.0.0.1" 7080 $ \pending -> do
    conn <- acceptRequest pending
    putStrLn "websocket connected"
    -- ping-pong every second
    void $ forkIO $ forever $ do
      threadDelay 1000000
      sendPing conn ("ping"::Text)
    -- synchronization
    void $ forkIO $ forever $ do
      t :: Text <- receiveData conn
      let (hsh,coords) :: (Int,[(Double,Double)]) = read (T.unpack t)
      -- putStrLn ("got " ++ show r)
      atomically $ do
        dat <- readTVar ref
        case getLast dat of
          Just (r,_,_) -> writeTVar ref (dat |> (r+1,hsh,coords))
          Nothing    -> writeTVar ref (dat |> (1,hsh,coords))

    void $ flip iterateM_ 0 $ \r -> do
      (r',hsh') <-
        atomically $ do
          dat <- readTVar ref
          case getLast dat of
            Just (r',hsh',_) -> if (r' <= r) then retry else pure (r',hsh')
            Nothing -> retry
      sendTextData conn (T.pack (show (r',hsh')))
      pure r'

  Warp.run 7070 $ serve api server
