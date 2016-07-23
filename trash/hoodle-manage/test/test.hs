{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Applicative
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logger (MonadLogger(..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Resource (MonadResource(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Data.Conduit (($$))
import qualified Data.Conduit.List as CL
-- import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Text.Encoding
import Database.Esqueleto
import Database.Persist.Sqlite (runSqlite)
import System.Directory
import System.FilePath
-- 
import Hoodle.Manage.Type.MD5
import Hoodle.Manage.Type.UUID
import Hoodle.Manage

-- import Crypto.Hash.MD5
-- import Data.ByteString.Base16 as B16

splitfunc :: String -> (String,(String,String))
splitfunc str = 
  let (str1,rest1) = break (==' ') str 
      (str2,rest2) = break (==' ') (tail rest1)
      str3 = read (tail rest2)
  in (str1,(str2,str3))

dbreadtest :: IO ()
dbreadtest = do
  tmpfile <- (</> "hoodleiddb.dat") <$> getCurrentDirectory 
  str <- readFile tmpfile 
  let strs = lines str
      assoclst = map splitfunc strs 
      -- assocmap = M.fromList assoclst 
  
  -- print assocmap 
  maintest assoclst



maintest :: [(String,(String,String))] -> IO () 
maintest assoclst = do 
  runSqlite ":memory:" $ do 
         buildDB assoclst 
         liftIO (putStrLn "dump hoodle")
         dumphdl
                 
buildDB :: (Monad m, MonadLogger m, MonadIO m, MonadResource m, MonadBaseControl IO m) => 
           [ (String,(String,String)) ]
        -> SqlPersistT m ()
buildDB amap = do
    runMigration migrateTables
    -- uuid <- liftIO $ nextRandom
    -- liftIO $   print (B16.encode (hash "dlkjfldfkfj"))
    -- let testmd5 = MD5 (hash "dlkjfldfkfj")
    -- insert $ HoodleFile uuid testmd5 "file://url.com/file.hdl"
    runMaybeT (mapM_ inserter amap)
    return ()

inserter :: (Monad m, MonadLogger m, MonadIO m, MonadResource m) => 
            (String,(String,String)) 
         -> MaybeT (SqlPersistT m) (Key (HoodleFileGeneric SqlBackend))
inserter (uuidstr, (md5str,fpstr)) = do 
  uuid <- (MaybeT . return . fromString) uuidstr
  let (decodedmd5,remained) = (B16.decode . encodeUtf8 . T.pack) md5str
  md5 <- if B.null remained then return (MD5 decodedmd5) else (MaybeT . return) Nothing
  let fp = T.pack fpstr
  lift . insert $ HoodleFile uuid md5 fp


dumphdl :: (MonadResource m, MonadLogger m) => SqlPersistT m ()
dumphdl = rawQuery "select * from Hoodle_File" [] $$ CL.mapM_ (liftIO . print)


main :: IO ()
main = do 
  putStrLn "hello"
  -- maintest
  dbreadtest