{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -w #-}

module Main where

import qualified Control.Exception as E
import Control.Monad (forever)
import qualified Data.ByteString.Lazy as BL
import GHC.RTS.Events.Incremental (readEvents, readHeader)
import Network.Socket
  ( Family (AF_UNIX),
    SockAddr (SockAddrUnix),
    Socket,
    SocketType (Stream),
    close,
    connect,
    socket,
    withSocketsDo,
  )
import Network.Socket.ByteString (recv)
import Text.Pretty.Simple (pPrint)

dump :: Socket -> IO ()
dump sock = goHeader ""
  where
    goHeader bs0 = do
      bs1 <- recv sock 1024
      let bs = bs0 <> bs1
      let lbs = BL.fromStrict bs
      let e = readHeader lbs
      case e of
        Left err -> print err >> goHeader bs
        Right (hdr, lbs') -> pPrint hdr >> goEvents hdr (BL.toStrict lbs')

    goEvents hdr bs0 = do
      bs1 <- recv sock 100000000
      let bs = bs0 <> bs1
      let lbs = BL.fromStrict bs
      let (evs, merr) = readEvents hdr lbs
      mapM_ pPrint evs
      print merr
      goEvents hdr ""

main :: IO ()
main = do
  withSocketsDo $ do
    let file = "/tmp/eventlog.sock"
        open = do
          sock <- socket AF_UNIX Stream 0
          connect sock (SockAddrUnix file)
          pure sock
    E.bracket open close dump
    pure ()
