{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE ScopedTypeVariables #-}
-- 
-- uuid,md5hash,filepath map utility  
-- 
import           Control.Applicative ((<$>))
import           Control.Lens
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Logger
import           Control.Monad.Trans.Either 
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Resource
import           Data.Attoparsec 
import           Data.Aeson.Parser (json)
import           Data.Aeson.Types  (parseJSON, parseEither)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import           Data.Conduit (($$), ($$+-))
import qualified Data.Conduit.List as CL 
import           Data.Data
import           Data.Digest.Pure.MD5
import qualified Data.List as DL 
import qualified Data.Map as M
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Database.Persist.Sqlite
import           Database.Persist.Sql (rawQuery)
import qualified Network.HTTP.Conduit as N
import           System.Console.CmdArgs
import           System.Directory
import           System.FilePath
import           System.Process
import           System.IO (stdin)
-- 
import           Data.Hoodle.Simple
import           Text.Hoodle.Parse.Attoparsec 
-- 
import           DiffDB
import           Migrate
import qualified Hoodle.Manage.SqliteDB as SqliteDB
import qualified TextFileDB
import           Util
import           Hoodle.Manage.DocDatabase
-- 


-- import Database.Persist.Sqlite
-- import Database.Persist.Sql (rawQuery)


data IdFilePathDB = List
                  | Info { fileid :: String }
                  | SingleFile { hoodlehome :: FilePath 
                               , singlefilename :: FilePath } 

                  | AllFiles { hoodlehome :: FilePath }
                  --  | DBDiff
                  --  | DBSync { remoteURL :: String 
                  --           , remoteID :: String 
                  --           , remotePassword :: String } 
                  --  | DBMigrateToSqlite
                  deriving (Show,Data,Typeable)

list :: IdFilePathDB 
list = List

info :: IdFilePathDB
info = Info { fileid = def &= typ "FILEID" &= argPos 0 }

singlefile :: IdFilePathDB 
singlefile = 
  SingleFile { hoodlehome = def &= typ "HOODLEHOME" &= argPos 0 
             , singlefilename = def &= typ "FILEPATH" &= argPos 1
             }
 
allfiles :: IdFilePathDB 
allfiles = 
  AllFiles { hoodlehome = def &= typ "HOODLEHOME" &= argPos 0 } 
  
{-
dbdiff :: IdFilePathDB 
dbdiff = DBDiff

dbsync :: IdFilePathDB 
dbsync = DBSync { remoteURL = def &= typ "URL" &= argPos 0
                , remoteID = def &= typ "ID" &= argPos 1
                , remotePassword = def &= typ "PASSWORD" &= argPos 2
                }

dbmigrate :: IdFilePathDB
dbmigrate = DBMigrateToSqlite
-}
  
mode :: IdFilePathDB
mode = modes [list, info, singlefile, allfiles] -- , singlefile, dbdiff, dbsync, dbmigrate ] 

main :: IO () 
main = do 
  params <- cmdArgs mode
  case params of
    List -> listwork
    Info uuid -> infowork uuid
    SingleFile hdir fp -> singlefilework hdir fp 
    AllFiles hdir      -> allfilework hdir 
    {- 
    DBDiff             -> dbdiffwork
    DBSync url idee pw -> dbsyncwork url idee pw
    DBMigrateToSqlite  -> dbmigrate2sqlite 
    -}

listwork :: IO ()
listwork = do
  dbfile <- SqliteDB.defaultDBFile
  runSqlite dbfile $ do 
    runMigration migrateTables
    dumpTable

infowork :: String -> IO ()
infowork uuid = do
  dbfile <- SqliteDB.defaultDBFile
  runSqlite dbfile $ do 
    runMigration migrateTables
    runMaybeT $ do 
      file <- (MaybeT . getBy . FileIDKey . T.pack) uuid
      liftIO (print file)
    return ()

singlefilework :: FilePath -> FilePath -> IO ()
singlefilework hdir oldfp = do 
  dbfile <- SqliteDB.defaultDBFile
  runSqlite dbfile $ do 
    runMigration migrateTables
    runMaybeT $ do 
      (uuid,md5str) <- MaybeT . liftIO $ checkHoodleIdMd5 oldfp 
      file <- (MaybeT . getBy . FileIDKey . T.pack) uuid
      liftIO (print file)
      update (entityKey file) [ HoodleDocLocationFilemd5 =. (T.pack md5str) ]
    return ()

allfilework :: FilePath -> IO ()
allfilework hdir = do 
  homedir <- getHomeDirectory 
  r <- readProcess "find" [homedir </> "Dropbox" </> "hoodle","-name","*.hdl","-print"] "" 
  mapM_ (singlefilework hdir) (lines r)

checkHoodleIdMd5 :: FilePath -> IO (Maybe (String,String))
checkHoodleIdMd5 fp = do 
  bstr <- B.readFile fp
  eh <- checkVersionAndGetIfHigherVersion bstr
  case eh of 
    Left str -> print str >> return Nothing 
    Right h -> do
      let idstr = B.unpack (view hoodleID h)
          md5str = show (md5 (L.fromChunks [bstr]))
      return (Just (idstr,md5str))
      

-- | using attoparsec without any built-in xml support 
checkVersionAndGetIfHigherVersion :: B.ByteString -> IO (Either String Hoodle) 
checkVersionAndGetIfHigherVersion bstr = do 
  case parseOnly checkHoodleVersion bstr of 
    Left str -> return (Left str )
    Right v -> do 
      if ( v < "0.1.9999" ) 
        then return (Left "low version") 
        else return (parseOnly hoodle bstr)


dbdiffwork :: IO ()
dbdiffwork = do 
  homedir <- getHomeDirectory 
  let newdbfile = homedir </> "Dropbox" </> "hoodleiddb.dat"
      olddbfile = homedir </> "Dropbox" </> "hoodleiddb.dat.old"
   
  newdbstr <- readFile newdbfile 
  olddbstr <- readFile olddbfile
  let makedb = M.fromList . map TextFileDB.splitfunc . lines 
      (newdb,olddb) = (makedb newdbstr, makedb olddbstr) 
  (L.putStrLn . encodePretty) (checkdiff olddb newdb)

dbsyncwork :: String -> String -> String -> IO ()
dbsyncwork url idee pwd = do           
    bstr <- B.hGetContents stdin
    request' <- N.parseUrl (url <> "/auth/page/hashdb/login")
    let crstr = B.pack ("username=" ++ idee ++ "&password=" ++ pwd)
        requestauth = request' 
          { N.method = "POST" 
          , N.requestHeaders = 
              ("Content-Type","application/x-www-form-urlencoded") 
              : N.requestHeaders request'   
          , N.requestBody = N.RequestBodyBS crstr
          } 
    mck <- runResourceT $ N.withManager $ \manager -> do  
             response <- N.http requestauth manager
             return (DL.lookup "Set-Cookie" (N.responseHeaders response))
    case mck of 
      Nothing -> return()
      Just ck -> do 
        request'' <- N.parseUrl (url <> "/syncdb")
        let requesttask = request'' 
              { N.method = "POST"
              , N.requestHeaders = ("Content-Type", "application/json") 
                                   : ("Cookie",ck) 
                                   : N.requestHeaders request''
              , N.requestBody = N.RequestBodyBS bstr
              }
        runResourceT $ N.withManager $ \manager -> do  
          response <- N.http requesttask manager
          content <- N.responseBody response $$+- CL.consume 
          liftIO $ print content  
   

