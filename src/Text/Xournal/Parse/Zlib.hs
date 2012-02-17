-----------------------------------------------------------------------------
-- |
-- Module      : Text.Xournal.Parse.Zlib 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Text.Xournal.Parse.Zlib where

import Control.Monad.IO.Class
import Data.Iteratee.ZLib as IZ
import Data.Iteratee as I
-- import Codec.Zlib.Enum as EZ
-- import Data.Enumerator as E
import Data.ByteString 
import qualified Data.ByteString.Lazy as LB

import System.IO

-- |

ungzipXoj :: MonadIO m => I.Enumerator ByteString m a
ungzipXoj = IZ.enumInflate IZ.GZip IZ.defaultDecompressParams

-- | 

-- gunzipXojEnum :: MonadIO m => E.Enumeratee ByteString ByteString m a
-- gunzipXojEnum = EZ.decompress EZ.defaultWindowBits  



-- | check if gzip or not

checkIfBinary :: FilePath -> IO Bool 
checkIfBinary fname = 
  withFile fname ReadMode $ \h -> do
    b <- return . LB.any ( == 0 ) . LB.take 100 =<< LB.hGetContents h 
    b `seq` return b 

