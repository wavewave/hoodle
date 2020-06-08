{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Concurrent (forkIO,threadDelay)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, retry, writeTVar)
import Control.Monad (forever,void)
import Control.Monad.Loops (iterateM_)
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

main :: IO ()
main = do
  putStrLn "servant server"
  ref <- newTVarIO (0 :: Int)

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
      let r :: Int = read (T.unpack t)
      putStrLn ("got " ++ show r)
      atomically (writeTVar ref r)

    void $ flip iterateM_ 0 $ \r -> do
      r' <- atomically $ do
              r' <- readTVar ref
              if (r' <= r)
                then retry
                else pure r'
      sendTextData conn (T.pack (show r'))
      pure r'

  Warp.run 7070 $ serve api server
