-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Util.Process
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Util.Process where

import Control.Concurrent
import Control.Monad
import Control.Monad.Loops 
import qualified Data.ByteString.Lazy as B
-- import Data.Monoid
-- import Data.UUID
import Data.UUID.V4 
import System.Directory
import System.FilePath
import System.IO
-- import System.IO.MMap
import System.Posix.Files
-- import System.Posix.Files.ByteString 
import System.Posix.IO
import System.Posix.Process 

-- | 
checkPipe :: FilePath -> IO ()
checkPipe fp = untilM_ (threadDelay 10000) (fileExist fp) 


-- | 
mkTmpFileName :: IO FilePath 
mkTmpFileName = do
  tdir <- getTemporaryDirectory 
  tuuid <- nextRandom
  return $ tdir </> show tuuid <.> "fifo"

-- | 
existThenRemove :: FilePath -> IO () 
existThenRemove fp = fileExist fp >>= \b -> when b (removeLink fp) 

-- |
pipeAction :: IO () -> (B.ByteString -> IO a) -> IO a 
pipeAction sender receiver = pipeActionWith sender (receiver <=< B.readFile)


{-  filename <- mkTmpFileName 
  existThenRemove filename 
  createNamedPipe filename (unionFileModes ownerReadMode ownerWriteMode)
  forkProcess $ do  
    fd <- openFd filename WriteOnly Nothing defaultFileFlags
    dupTo fd stdOutput 
    closeFd fd
    sender 
    hFlush stdout 
  r <- receiver =<< B.readFile filename << checkPipe filename 
  removeLink filename  
  return r 
-}

-- |
pipeActionWith :: IO () -> (FilePath -> IO a) -> IO a 
pipeActionWith sender receiverf = do 
  filename <- mkTmpFileName 
  existThenRemove filename 
  createNamedPipe filename (unionFileModes ownerReadMode ownerWriteMode)
  forkProcess $ do  
    fd <- openFd filename WriteOnly Nothing defaultFileFlags
    dupTo fd stdOutput 
    closeFd fd
    sender 
    hFlush stdout 
  r <- checkPipe filename >> receiverf filename  
  removeLink filename  
  return r 

{-
-- | 
pipeActionSystem :: String -> (B.ByteString -> IO a) -> IO a 
pipeActionSystem sendercmd receiver = do 
  filename <- mkTmpFileName 
  existThenRemove filename 
  createNamedPipe filename (unionFileModes ownerReadMode ownerWriteMode)
-}