module Text.Hoodle.Parse.Util where

import Control.Monad.IO.Class
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
--
import Data.Hoodle.Simple
import System.Directory
--
import qualified Text.Hoodle.Parse.Attoparsec as PA

-- |
withHoodle :: (MonadIO m, Functor m) => FilePath -> (Hoodle -> m a) -> m (Maybe a)
withHoodle fname act = do
  b <- liftIO $ doesFileExist fname
  if not b
    then return Nothing
    else do
      bstr <- liftIO $ B.readFile fname
      case parseOnly PA.hoodle bstr of
        Left _err -> return Nothing
        Right hdl -> Just <$> act hdl
