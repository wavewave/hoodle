{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Migrate where

import           Control.Applicative
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import           Data.Conduit (($$))
import qualified Data.Conduit.List as CL 
import qualified Data.Text as T
import           Database.Persist.Sqlite
import           Database.Persist.Sql (rawQuery)
import           System.Directory
import           System.FilePath
--
import           Hoodle.Manage.DocDatabase
-- 
import qualified SqliteDB
import qualified TextFileDB
import           Util

dbmigrate2sqlite :: IO ()
dbmigrate2sqlite = do
  putStrLn "migration test"
  origdbfile <- TextFileDB.defaultDBFile
  newdbfile <- T.unpack <$> SqliteDB.defaultDBFile -- homedir </> ".hoodle.d" </> "hoodleiddb.dat"
  str <- readFile origdbfile
  let assoclst = (map TextFileDB.splitfunc . lines) str 
      assoclst' = map (\(x,(y,z)) -> (T.pack x, (T.pack y, T.pack z))) assoclst 
  runSqlite (T.pack newdbfile) $ do 
    runMigration migrateTables
    mapM_ insertOne assoclst'
    dumpTable

insertOne :: (MonadBaseControl IO m, MonadIO m, MonadThrow m) => 
             (T.Text, (T.Text, T.Text)) 
          -> SqlPersistT (NoLoggingT (ResourceT m)) (Key (HoodleDocLocationGeneric SqlBackend))
insertOne (x,(y,z)) = insert (HoodleDocLocation x y z)
