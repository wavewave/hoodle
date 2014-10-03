{-# LANGUAGE ScopedTypeVariables #-}

-- {-# LANGUAGE OverloadedStrings #-}

module Hub where

import Control.Concurrent
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Trans.Maybe
import Data.Monoid
import qualified Data.Text as T
import Network.Simple.TCP
import System.Environment
import System.FilePath
import System.Process
--
import Message


--     unfoldM_ $ do 

receiveHub :: IO ()
receiveHub = do
  hubaddr <- getEnv "HUBSOCKETADDRESS"
  hoodlehome <- getEnv "HOODLEHOME"
  connect hubaddr "5051" $ \(sock, servaddr) -> do 
    putStrLn $ "client: connection established to " ++ show servaddr
    unfoldM_ $ do 
      mmsg :: Maybe Message <- runMaybeT $ recvAndUnpack sock
      case mmsg of
        Nothing -> return ()
        Just msg -> do 
          createProcess (proc "hoodle" [hoodlehome </> T.unpack (msgbody msg)]) >> return ()
          packAndSend sock "hoodle started"
      return mmsg 
