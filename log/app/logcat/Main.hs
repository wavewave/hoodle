{-# OPTIONS_GHC -w #-}

module Main where

import qualified Control.Exception as E
import Control.Monad (forever)
import Network.Socket
  ( Family (AF_UNIX),
    SockAddr (SockAddrUnix),
    SocketType (Stream),
    close,
    connect,
    socket,
    withSocketsDo,
  )
import Network.Socket.ByteString (recv)

main :: IO ()
main = do
  withSocketsDo $ do
    let file = "/tmp/eventlog.sock"
        open = do
          sock <- socket AF_UNIX Stream 0
          connect sock (SockAddrUnix file)
          pure sock
    E.bracket open close $ \sock -> forever $ do
      _ <- recv sock 1024
      putStrLn "i got it!"
    pure ()
