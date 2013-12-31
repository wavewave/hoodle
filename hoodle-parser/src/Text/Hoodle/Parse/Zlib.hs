-----------------------------------------------------------------------------
-- |
-- Module      : Text.Hoodle.Parse.Zlib 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Text.Hoodle.Parse.Zlib where

-- from other packages
import qualified Data.ByteString.Lazy as LB
import           System.IO

-- | check if gzip or not

checkIfBinary :: FilePath -> IO Bool 
checkIfBinary fname = 
  withFile fname ReadMode $ \h -> do
    b <- return . LB.any ( == 0 ) . LB.take 100 =<< LB.hGetContents h 
    b `seq` return b 

