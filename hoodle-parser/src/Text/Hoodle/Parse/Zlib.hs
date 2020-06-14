module Text.Hoodle.Parse.Zlib where

import qualified Data.ByteString.Lazy as LB
import System.IO

-- | check if gzip or not
checkIfBinary :: FilePath -> IO Bool
checkIfBinary fname =
  withFile fname ReadMode $ \h -> do
    b <- return . LB.any (== 0) . LB.take 100 =<< LB.hGetContents h
    b `seq` return b
