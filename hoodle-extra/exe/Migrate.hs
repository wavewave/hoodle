{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Migrate where

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
import           TextFileDB

dbmigrate2sqlite :: IO ()
dbmigrate2sqlite = do
  putStrLn "migration test"
  homedir <- getHomeDirectory 
  let origdbfile = homedir </> "Dropbox" </> "hoodleiddb.dat"
      newdbfile = homedir </> ".hoodle.d" </> "hoodleiddb.dat"
  str <- readFile origdbfile
  let assoclst = (map splitfunc . lines) str 
      assoclst' = map (\(x,(y,z)) -> (T.pack x, (T.pack y, T.pack z))) assoclst 
  runSqlite (T.pack newdbfile) $ do 
    runMigration migrateTables
    mapM_ insertOne assoclst'
    dumpTable

dumpTable :: (MonadBaseControl IO m, MonadIO m, MonadThrow m) => SqlPersistT (NoLoggingT (ResourceT m)) ()
dumpTable = rawQuery "select * from hoodle_doc_location" [] $$ CL.mapM_ (liftIO . print)


insertOne :: (MonadBaseControl IO m, MonadIO m, MonadThrow m) => 
             (T.Text, (T.Text, T.Text)) 
          -> SqlPersistT (NoLoggingT (ResourceT m)) (Key (HoodleDocLocationGeneric SqlBackend))
insertOne (x,(y,z)) = insert (HoodleDocLocation x y z)
