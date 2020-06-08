{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Concurrent (forkIO,threadDelay)
import Control.Monad (forever,void)
import Data.Text (Text)
import qualified Network.Wai.Handler.Warp as Warp
import Network.WebSockets ( acceptRequest, runServer, sendPing, sendTextData )
import Servant ( (:>), Get, JSON, Proxy(..), Server, serve )

type API = "hello" :> Get '[JSON] String

api :: Proxy API
api = Proxy

server :: Server API
server = pure "hello world"

main :: IO ()
main = do
  putStrLn "servant server"

  void $ forkIO $ runServer "127.0.0.1" 7080 $ \pending -> do
    conn <- acceptRequest pending
    putStrLn "websocket connected"
    void $ forever $ do
      threadDelay 1000000
      sendTextData conn ("ping"::Text)

  Warp.run 7070 $ serve api server
