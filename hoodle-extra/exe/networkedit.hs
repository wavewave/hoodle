{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative ((<$>))
import           Control.Concurrent
import           Control.Monad (when,replicateM)
import           Control.Monad.Loops (unfoldM) 
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import qualified Data.Binary as Bi
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Foldable as F (mapM_,forM_)
import           Data.Maybe (catMaybes)
import           Data.Monoid
import           Data.String
import           Data.Time.Clock
-- import           Data.UUID.V4
import           Data.Word
import qualified Filesystem.Path as FP
import           Network.Simple.TCP 
import           System.Directory
import           System.FilePath
import           System.FSNotify
import           System.Environment
import           System.Process

main :: IO ()
main = do
  [arg0,arg1] <- getArgs
  ed <- getEnv "EDITOR"
  connect arg0 arg1 $ \(sock, addr) -> do
    putStrLn $ "Connection established to " ++ show addr
    mr <- runMaybeT $ do 
      bstr <- MaybeT (recv sock 4)
      let getsize :: B.ByteString -> Word32 
          getsize = Bi.decode . LB.fromChunks . return
          size = (fromIntegral . getsize) bstr 
          go s bstr = do 
            bstr1 <- MaybeT (recv sock s)
            let s' = B.length bstr1 
            if s <= s' 
              then return (bstr <> bstr1)
              else go (s-s') (bstr <> bstr1) 
      go size B.empty 
    F.forM_ mr $ \bstr -> do 
      tdir <- getTemporaryDirectory
      ctime <- getCurrentTime
      let fpath = tdir </> show ctime <.> "txt"
      let edws = words ed 
          ed1 = head edws
          eds = tail edws
      print (ed1, [eds ++ [fpath] ])   -- just in case we have some error
      B.writeFile fpath bstr
      -- mvar <- newMVar ()
      -- takeMVar mvar 
      forkIO $ do 
        withManager $ \wm -> do
          -- putStrLn "watching start" 
          watchDir wm (fromString tdir) (\case Modified _ _ -> True; _ -> False) $ \event -> do
            -- print event
            case event of
              Modified fp _ -> do
                when (fromString fpath == fp) $ sendToHoodle sock fpath
              _ -> return ()
          threadDelay (3600*1000000)

      (_,_,_,h) <- createProcess (proc ed1 (eds ++ [fpath]))
      waitForProcess h
      sendToHoodle sock fpath
      return ()
      

sendToHoodle :: Socket -> FilePath -> IO ()
sendToHoodle sock fpath = do
    nbstr <- B.readFile fpath
    let nbstr_size :: Word32 = (fromIntegral . B.length) nbstr
        nbstr_size_binary = (mconcat . LB.toChunks . Bi.encode) nbstr_size
    send sock (nbstr_size_binary <> nbstr)
