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

import           Control.Applicative
import           Control.Concurrent hiding (yield)
-- import           Control.Distributed.Process 
-- import           Control.Distributed.Process.Global
-- import           Control.Distributed.Process.Node 
import           Control.Exception
import           Control.Lens 
import           Control.Monad (forever,unless)
import           Control.Monad.State (modify,get)
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Data.ByteString.Char8 as B
import qualified Data.Foldable as F (mapM_)
import           Data.Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Graphics.UI.Gtk hiding (get,set)
-- import           Network.Transport (Transport(..),closeTransport)
-- import           Network.Transport.TCP (createTransport,defaultTCPParameters)
import           Network.Simple.TCP
import           Pipes
import           Pipes.Network.TCP
import           System.IO (isEOF)
-- 
import           Control.Monad.Trans.Crtn.Event 
import           Control.Monad.Trans.Crtn.Queue (enqueue)
-- 
import           Hoodle.Coroutine.Dialog
import           Hoodle.Coroutine.Draw
import           Hoodle.Script.Hook
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Enum
import           Hoodle.Type.Event
import           Hoodle.Type.HoodleState (tempQueue,hookSet)
-- 

stdinLn :: Producer String IO ()
stdinLn = do
    eof <- lift isEOF 
    unless eof $ do
      str <- lift getLine
      yield str
      stdinLn


server :: (AllEvent -> IO ()) -> HostPreference -> T.Text -> IO ()
server evhandler ip txt = do
  listen ip  "4040" $ \(lsock, _) -> 
    accept lsock $ \(sock,addr) -> do 
      putStrLn $ "TCP connection established from " ++ show addr
      send sock (TE.encodeUtf8 txt)
      mbstr <- recv sock 100000
      F.mapM_ (evhandler . UsrEv . NetworkProcess . NetworkReceived . TE.decodeUtf8) mbstr

networkTextInput :: T.Text -> MainCoroutine (Maybe T.Text)
networkTextInput txt = do 
    mipscr <- runMaybeT $ do hkset <- MaybeT (view hookSet <$> lift get)
                             (MaybeT . return)  (getIPaddress hkset)
    ip <- maybe (return "127.0.0.1") liftIO mipscr 

    doIOaction $ \evhandler -> do  
      -- T.putStrLn txt
      done <- newEmptyMVar
      tid <- forkIO (server evhandler (Host ip) txt) 
      (return . UsrEv . NetworkProcess) (NetworkInitialized tid done)
    let go = do 
          r <- nextevent
          case r of
            UpdateCanvas cid -> invalidateInBBox Nothing Efficient cid >> go
            NetworkProcess (NetworkInitialized tid done) -> return (tid,done)
            _ -> go 
    (tid,done) <- go 
    let ipdialog msg = mkIOaction $ 
               \_evhandler -> do                  
                 dialog <- messageDialogNew Nothing [DialogModal]
                   MessageQuestion ButtonsOkCancel msg 
                 forkIO $ do          
                   readMVar done 
                   dialogResponse dialog ResponseOk
                  
                 res <- dialogRun dialog 
                 let b = case res of 
                           ResponseOk -> True
                           _ -> False
                 widgetDestroy dialog 
                 return (UsrEv (OkCancel b))

    
    modify (tempQueue %~ enqueue (ipdialog ("networkedit " ++ ip ++ " 4040")))
    --
    let act txt = do 
          r <- nextevent
          case r of 
            UpdateCanvas cid -> invalidateInBBox Nothing Efficient cid 
                                >> act txt
            OkCancel True -> (return . Just) txt
            OkCancel False -> return Nothing
            NetworkProcess (NetworkReceived txt') ->  do 
              doIOaction $ \_ -> postGUISync (putMVar done ())
                                 >> (return . UsrEv . NetworkProcess) NetworkCloseDialog
              act txt' 
              
            _ -> act txt
    ntxt <- act txt
    --   
    doIOaction $ \_evhandler -> do  
      killThread tid
      (return . UsrEv . NetworkProcess) NetworkClosed
    --
    return ntxt

