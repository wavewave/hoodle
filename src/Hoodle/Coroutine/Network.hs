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
import           Control.Monad.State (modify)
import           Control.Monad.Trans
import qualified Data.ByteString.Char8 as B
import qualified Data.Foldable as F (mapM_)
import           Data.Map
import           Graphics.UI.Gtk
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
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Enum
import           Hoodle.Type.Event
import           Hoodle.Type.HoodleState (tempQueue)
-- 

stdinLn :: Producer String IO ()
stdinLn = do
    eof <- lift isEOF 
    unless eof $ do
      str <- lift getLine
      yield str
      stdinLn


server :: (AllEvent -> IO ()) -> String -> IO ()
server evhandler str = do
  listen (Host "192.168.1.15") "4040" $ \(lsock, _) -> 
    accept lsock $ \(sock,addr) -> do 
      putStrLn $ "TCP connection established from " ++ show addr
      send sock (B.pack str)
      mbstr <- recv sock 100000
      F.mapM_ (evhandler . UsrEv . MultiLine . MultiLineChanged . B.unpack) mbstr

{-
textViewDialog :: String -> Either (ActionOrder AllEvent) AllEvent
textViewDialog str = mkIOaction $ \evhandler -> do
    dialog <- dialogNew
    vbox <- dialogGetUpper dialog
    -- 
    table <- tableNew 2 2 False
    tableAttachDefaults table textarea 0 1 0 1
    tableAttachDefaults table vscrbar 1 2 0 1
    tableAttachDefaults table hscrbar 0 1 1 2 
    boxPackStart vbox table PackNatural 0
    -- 
    btnOk <- dialogAddButton dialog "Ok" ResponseOk
    btnCancel <- dialogAddButton dialog "Cancel" ResponseCancel
    widgetShowAll dialog
    res <- dialogRun dialog
    widgetDestroy dialog
    case res of 
      ResponseOk -> return (UsrEv (OkCancel True))
      ResponseCancel -> return (UsrEv (OkCancel False))
      _ -> return (UsrEv (OkCancel False))
-}

networkTextInput :: String -> MainCoroutine (Maybe String)
networkTextInput str = do 
    doIOaction $ \evhandler -> do  
      print str 
      tid <- forkIO (server evhandler str) 
      (return . UsrEv . NetworkProcess . NetworkInitialized) tid
    let go = do r <- nextevent
                case r of
                  UpdateCanvas cid -> invalidateInBBox Nothing Efficient cid >> go
                  NetworkProcess (NetworkInitialized tid) -> return tid
                  _ -> go 
    tid <- go 
    let ipdialog msg = mkIOaction $ 
               \_evhandler -> do 
                 dialog <- messageDialogNew Nothing [DialogModal]
                   MessageQuestion ButtonsOkCancel msg 
                 res <- dialogRun dialog 
                 let b = case res of 
                           ResponseOk -> True
                           _ -> False
                 widgetDestroy dialog 
                 return (UsrEv (OkCancel b))

    
    modify (tempQueue %~ enqueue (ipdialog "192.168.1.15:4040"))
    --
    let act str = do 
          r <- nextevent
          case r of 
            UpdateCanvas cid -> invalidateInBBox Nothing Efficient cid 
                                >> act str
            OkCancel True -> (return . Just) str
            OkCancel False -> return Nothing
            MultiLine (MultiLineChanged str') -> act str' 
            _ -> act str
    nstr <- act str
    --   
    doIOaction $ \_evhandler -> do  
      killThread tid
      (return . UsrEv . NetworkProcess) NetworkClosed
    --
    return nstr

