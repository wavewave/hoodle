{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Network
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Network where

import           Control.Concurrent
import qualified Control.Distributed.Process as P
import qualified Control.Distributed.Process.Node as N
import           Control.Exception
import           Control.Lens 
import           Control.Monad.Trans
import           Data.Map
import           Network.Transport
import           Network.Transport.TCP (createTransport, defaultTCPParameters)
-- 
import           Hoodle.Coroutine.Dialog
import           Hoodle.Coroutine.Draw
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Enum
import           Hoodle.Type.Event


echoServer :: EndPoint -> MVar () -> IO ()
echoServer endpoint serverDone = go empty
  where
    go :: Map ConnectionId (MVar Connection) -> IO ()
    go cs = do
      event <- receive endpoint
      case event of
        ConnectionOpened cid rel addr -> do
          putStrLn $ "New connection: ID " ++ show cid ++ ", reliability: " ++ show rel ++ ", address: " ++ show addr
          connMVar <- newEmptyMVar
          forkIO $ do 
            Right conn <- connect endpoint addr rel defaultConnectHints
            putMVar connMVar conn
          go (insert cid connMVar cs)
        Received cid payload -> do
          forkIO $ do
            conn <- readMVar (cs ! cid)
            send conn payload
            return ()
          go cs
        ConnectionClosed cid -> do
          putStrLn $ "Closed connection: ID " ++ show cid
          forkIO $ do 
            conn <- readMVar (cs ! cid)
            close conn
          go (delete cid cs)
        EndPointClosed -> do
          putStrLn "Echo server exiting"
          putMVar serverDone ()
          
          
{-
onCtrlC :: IO a -> IO () -> IO a
p `onCtrlC` q = catchJust isUserInterrupt p (const $ q >> p `onCtrlC` q)
  where isUserInterrupt :: AsyncException -> Maybe ()
        isUserInterrupt UserInterrupt = Just ()
        isUserInterrupt _             = Nothing
-}

networkTextInput :: String -> MainCoroutine (Maybe String)
networkTextInput str = do 
    doIOaction $ \_evhandler -> do  
      serverDone <- newEmptyMVar      
      forkIO $ do 
        let (host,port) = ("192.168.1.15","10501")
        Right transport <- createTransport host port defaultTCPParameters 
        Right endpoint <- newEndPoint transport
        forkIO $ echoServer endpoint serverDone
        putStrLn $ "Echo server started at " ++ show (address endpoint)
        readMVar serverDone 
        closeTransport transport
        putStrLn $ "Echo server end"
        return ()
      (return . UsrEv . NetworkProcess . NetworkInitialized) serverDone
    let go = do r <- nextevent
                case r of
                  UpdateCanvas cid -> -- this is temporary
                    invalidateInBBox Nothing Efficient cid >> go
                  NetworkProcess (NetworkInitialized var) -> return var
                  _ -> go 
    serverDone <- go 
    b <- okCancelMessageBox "server started" 
    
    doIOaction $ \_evhandler -> do  
      putMVar serverDone ()
      (return . UsrEv . NetworkProcess) NetworkClosed
    
    return Nothing

