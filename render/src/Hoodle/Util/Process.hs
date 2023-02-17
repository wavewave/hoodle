-- TODO: This file should be moved to hoodle-util.
{- ORMOLU_DISABLE -}
{-# Language CPP #-}
module Hoodle.Util.Process where

import Control.Concurrent (threadDelay)
import Control.Monad (when, (<=<))
import Control.Monad.Loops (untilM_)
import qualified Data.ByteString.Lazy as B
import Data.UUID.V4 (nextRandom)
import System.Directory (getTemporaryDirectory)
import System.FilePath ((<.>), (</>))
import System.IO (hFlush, stdout)
import System.Posix.Files
  ( createNamedPipe,
    fileExist,
    ownerReadMode,
    ownerWriteMode,
    removeLink,
    unionFileModes,
  )
import System.Posix.IO
  ( OpenMode (WriteOnly),
    closeFd,
    defaultFileFlags,
    dupTo,
    openFd,
    stdOutput,
  )
import System.Posix.Process (forkProcess)

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

-- |
pipeActionWith :: IO () -> (FilePath -> IO a) -> IO a
pipeActionWith sender receiverf = do
  filename <- mkTmpFileName
  existThenRemove filename
  createNamedPipe filename (unionFileModes ownerReadMode ownerWriteMode)
  _ <- forkProcess $ do
#if MIN_VERSION_unix(2, 8, 0)
    fd <- openFd filename WriteOnly defaultFileFlags
#else
    fd <- openFd filename WriteOnly Nothing defaultFileFlags
#endif
    _ <- dupTo fd stdOutput
    closeFd fd
    sender
    hFlush stdout
  r <- checkPipe filename >> receiverf filename
  removeLink filename
  return r
