
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
module Text.Xournal.Parse.Zlib where

import Control.Monad.IO.Class
import Data.Iteratee.ZLib 
import Data.Iteratee
import Data.ByteString

ungzipXoj :: MonadIO m => Enumerator ByteString m a
ungzipXoj = enumInflate GZip defaultDecompressParams 

