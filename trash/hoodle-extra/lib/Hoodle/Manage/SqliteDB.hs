module Hoodle.Manage.SqliteDB where 

import           Control.Applicative
import qualified Data.Text as T
import           System.Directory
import           System.FilePath

defaultDBFile :: IO T.Text
defaultDBFile = (T.pack . (</> ".hoodle.d" </> "hoodleiddb.dat")) <$> getHomeDirectory
