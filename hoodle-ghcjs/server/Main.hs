{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Network.Wai.Handler.Warp as Warp
import Servant ( (:>), Get, JSON, Proxy(..), Server, serve )

type API = "hello" :> Get '[JSON] String

api :: Proxy API
api = Proxy

server :: Server API
server = pure "hello world"

main :: IO ()
main = do
  putStrLn "servant server"
  Warp.run 7070 $ serve api server
