module Text.Xournal.Parse.Zlib where

import Control.Monad.IO.Class
import Data.Iteratee.ZLib 
import Data.Iteratee
import Data.ByteString
-- import Codec.Zlib.Enum
-- import Data.Enumerator
-- import Data.ByteString 

ungzipXoj :: MonadIO m => Enumerator ByteString m a
ungzipXoj = enumInflate GZip defaultDecompressParams 

