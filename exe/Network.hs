{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Network
-- Copyright   : (c) 2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Network where

import           Control.Concurrent hiding (yield)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Data.Binary as Bi 
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Foldable as F (forM_)
import           Data.Monoid ((<>),mconcat)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Word
import           Network.Info
import           Network.Simple.TCP

-- 
-- 
laTeXHeader :: T.Text
laTeXHeader = "\\documentclass{article}\n\
              \\\pagestyle{empty}\n\
              \\\begin{document}\n"
                                
laTeXFooter :: T.Text
laTeXFooter = "\\end{document}\n"

defaultText :: T.Text 
defaultText = laTeXHeader <> "\n\n" <> laTeXFooter


server :: HostPreference -> T.Text -> Chan T.Text -> IO ()
server ip txt chan = do
  listen ip  "4041" $ \(lsock, _) -> 
    accept lsock $ \(sock,addr) -> do 
      let bstr = TE.encodeUtf8 txt
          bstr_size :: Word32 = (fromIntegral . B.length) bstr 
          bstr_size_binary = (mconcat . LB.toChunks . Bi.encode) bstr_size
      -- B.putStrLn () 
      putStrLn $ "TCP connection established from " ++ show addr
      send sock (bstr_size_binary <> TE.encodeUtf8 txt)
      
      mbstr <- runMaybeT $ do 
        bstr' <- MaybeT (recv sock 4)
        let getsize :: B.ByteString -> Word32 
            getsize = Bi.decode . LB.fromChunks . return
            size = (fromIntegral . getsize) bstr'

            go s bs = do 
              bstr1 <- MaybeT (recv sock s)
              let s' = B.length bstr1 
              if s <= s' 
                then return (bs <> bstr1)
                else go (s-s') (bs <> bstr1) 
        go size B.empty 
      F.forM_ mbstr $ \rbstr -> writeChan chan (TE.decodeUtf8 rbstr)
      print mbstr 

ipfind :: IO String
ipfind = do 
    let ipv4num (IPv4 x) = x 
        ismacnull (MAC a b c d e f) = a == 0 && b == 0 && c == 0 
                                      && d == 0 && e == 0 && f == 0 
          
    ifcs <- getNetworkInterfaces
    let ifcs2 = Prelude.filter (not . ismacnull . mac) 
                . Prelude.filter (((/=) 0) . ipv4num . ipv4 ) $ ifcs
    return (if Prelude.null ifcs2 then "127.0.0.1" else (show . ipv4 . head) ifcs2) 
    
    




{- 
networkTextInput :: T.Text -> MainCoroutine (Maybe T.Text)
networkTextInput txt = do 
    mipscr <- runMaybeT $ do hkset <- MaybeT (view hookSet <$> lift get)
                             (MaybeT . return)  (getIPaddress hkset) 
    
    let ipfind = do 
          let ipv4num (IPv4 x) = x 
              ismacnull (MAC a b c d e f) = a == 0 && b == 0 && c == 0 
                                            && d == 0 && e == 0 && f == 0 
          
          ifcs <- liftIO $ getNetworkInterfaces
          let ifcs2 = Prelude.filter (not . ismacnull . mac) 
                      . Prelude.filter (((/=) 0) . ipv4num . ipv4 ) $ ifcs
          return (if Prelude.null ifcs2 then "127.0.0.1" else (show . ipv4 . head) ifcs2) 
    ip <- maybe ipfind liftIO mipscr 

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
    let actf t = do 
          r <- nextevent
          case r of 
            UpdateCanvas cid -> invalidateInBBox Nothing Efficient cid 
                                >> actf t
            OkCancel True -> (return . Just) t
            OkCancel False -> return Nothing
            NetworkProcess (NetworkReceived txt') ->  do 
              doIOaction $ \_ -> postGUISync (putMVar done ())
                                 >> (return . UsrEv . NetworkProcess) NetworkCloseDialog
              actf txt' 
              
            _ -> actf t
    ntxt <- actf txt
    --   
    doIOaction $ \_evhandler -> do  
      killThread tid
      (return . UsrEv . NetworkProcess) NetworkClosed
    --
    return ntxt
-}
