{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}


import           Control.Monad.IO.Class (liftIO) 
import           Data.Conduit (($$))
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite
import           System.IO (stdout, hPutStrLn, IOMode(..))
-- 
import           Hoodle.Manage.DocDatabase
import qualified Hoodle.Manage.SqliteDB as SqliteDB

main :: IO ()
main = do
  dbfile <- SqliteDB.defaultDBFile
  putStrLn $ "loading db " ++ show dbfile
  runSqlite dbfile $ do 
    runMigration migrateTables
    all <- selectList ([] :: [Filter HoodleDocLocation]) []
    liftIO $ mapM_ (formatter stdout) all 


formatter h x = do let v = entityVal x
                       i = hoodleDocLocationFileid v
                       m = hoodleDocLocationFilemd5 v
                       l = hoodleDocLocationFileloc v
                   hPutStrLn h (T.unpack i ++ " " ++ T.unpack m ++ " " ++ show l)
                  
-- dumpTable :: Double
-- dumpTable = rawQuery "select * from hoodle_doc_location" [] $$ CL.mapM_ (liftIO . print)
