{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

-- 
-- uuid,md5hash,filepath map utility  
-- 
import           Control.Applicative
import           Control.Lens hiding (List)
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Logger
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either 
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Resource
import           Data.Attoparsec 
import           Data.Aeson (decode)
import           Data.Aeson.Parser (json)
import           Data.Aeson.Types  (parseJSON, parseEither,ToJSON (..),FromJSON (..), Value(..),object, (.:) )
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import           Data.Conduit (($$), ($$+-))
import qualified Data.Conduit.List as CL 
import           Data.Data
import           Data.Digest.Pure.MD5
import qualified Data.HashMap.Strict as H
import qualified Data.List as DL 
import qualified Data.Map as M
import           Data.Monoid ((<>),mconcat)
import qualified Data.Text as T
import           Database.Persist.Sqlite
import           Database.Persist.Sql (rawQuery)
import qualified Network.HTTP.Conduit as N
import           System.Console.CmdArgs
import           System.Directory
import           System.FilePath
import           System.Process
import           System.IO (stdin,stdout,hGetContents,hPutStrLn)
-- 
import           Data.Hoodle.Simple
import           Text.Hoodle.Parse.Attoparsec 
-- 
import           Hoodle.Manage.Connect
import           Hoodle.Manage.DocDatabase
import           DiffDB
-- import           Migrate
import qualified Hoodle.Manage.SqliteDB as SqliteDB
import qualified TextFileDB
import           Util
-- 


data IdFilePathDB = List
                  | Info { fileid :: String }
                  | SingleFile { hoodlehome :: Maybe FilePath 
                               , singlefilename :: FilePath } 

                  | AllFiles { hoodlehome :: Maybe FilePath }
                  | ChangeRoot { newhome :: FilePath }
                  | DBDiff { newfile :: FilePath }
                  | DBSync { remoteURL :: String 
                           , remoteID :: String 
                           , remotePassword :: String } 
                  | DBGet  { remoteURL :: String 
                           , remoteID :: String
                           , remotePassword :: String
                           }
                  --  | DBMigrateToSqlite
                  deriving (Show,Data,Typeable)

list :: IdFilePathDB 
list = List

info :: IdFilePathDB
info = Info { fileid = def &= typ "FILEID" &= argPos 0 }

singlefile :: IdFilePathDB 
singlefile = 
  SingleFile { hoodlehome = def
             , singlefilename = def &= typ "FILEPATH" &= argPos 0
             }
 
allfiles :: IdFilePathDB 
allfiles = 
  AllFiles { hoodlehome = def } 

changeroot :: IdFilePathDB 
changeroot = 
  ChangeRoot { newhome = def &= typ "HOODLEHOME" &= argPos 0 } 


dbsync :: IdFilePathDB 
dbsync = DBSync { remoteURL = def &= typ "URL" &= argPos 0
                , remoteID = def &= typ "ID" &= argPos 1
                , remotePassword = def &= typ "PASSWORD" &= argPos 2
                }
  

dbdiff :: IdFilePathDB 
dbdiff = DBDiff { newfile = def &= typ "FILE" &= argPos 0 } 

dbget :: IdFilePathDB
dbget = DBGet { remoteURL = def &= argPos 0 
              , remoteID = def &= argPos 1
              , remotePassword = def &= argPos 2
              }

{-
dbmigrate :: IdFilePathDB
dbmigrate = DBMigrateToSqlite
-}
  
mode :: IdFilePathDB
mode = modes [list, info, singlefile, allfiles, changeroot, dbsync, dbdiff, dbget ] -- , singlefile, dbmigrate ] 


listwork :: IO ()
listwork = do
  dbfile <- SqliteDB.defaultDBFile
  runSqlite dbfile $ do 
    runMigration migrateTables
    runMigration migrateDocRoot
    all <- selectList ([] :: [Filter HoodleDocLocation]) []
    liftIO $ mapM_ (formatter stdout) all 
  where formatter h x = do 
          let v = entityVal x
              i = hoodleDocLocationFileid v
              m = hoodleDocLocationFilemd5 v
              l = hoodleDocLocationFileloc v
          hPutStrLn h (T.unpack i ++ " " ++ T.unpack m ++ " " ++ show l)

infowork :: String -> IO ()
infowork uuid = do
  dbfile <- SqliteDB.defaultDBFile
  runSqlite dbfile $ do 
    runMigration migrateTables
    runMaybeT $ do 
      file <- (MaybeT . getBy . FileIDKey . T.pack) uuid
      liftIO (print file)
    return ()

singlefilework :: Maybe FilePath -> FilePath -> IO ()
singlefilework mhdir fp = do 
  canfp <- canonicalizePath fp
  dbfile <- SqliteDB.defaultDBFile
  print dbfile
  runSqlite dbfile $ do 
    runMigration migrateTables
    runMigration migrateDocRoot
    mhdir2 <- case mhdir of
                Just hdir' -> return (Just hdir')
                Nothing -> fmap (T.unpack . hoodleDocRootLoc . entityVal ) <$>  getBy (UniqueDocRoot True)
    liftIO $ print mhdir2
    runMaybeT $ do 
      hdir <- (MaybeT . return) mhdir2
      let relfp = makeRelative hdir canfp
      (uuid,md5str) <- MaybeT . liftIO $ checkHoodleIdMd5 canfp 
      liftIO $ print uuid
      mfile <- (lift . getBy . FileIDKey . T.pack) uuid
      liftIO (print mfile)
      case mfile of 
        Nothing -> lift $ insert (HoodleDocLocation (T.pack uuid) (T.pack md5str) (T.pack relfp)) >> return ()
        Just file -> lift $ update (entityKey file) [ HoodleDocLocationFilemd5 =. (T.pack md5str), HoodleDocLocationFileloc =. (T.pack relfp) ] >> return ()
    return ()

allfilework :: Maybe FilePath -> IO ()
allfilework mhdir = do 
  homedir <- getHomeDirectory 
  r <- readProcess "find" [homedir </> "Dropbox" </> "hoodle","-name","*.hdl","-print"] "" 
  mapM_ (singlefilework mhdir) (lines r)
 
changerootwork :: FilePath -> IO ()
changerootwork hdir = do
  dbfile <- SqliteDB.defaultDBFile
  runSqlite dbfile $ do 
    runMigration migrateDocRoot
    mroot <- getBy (UniqueDocRoot True)
    case mroot of
      Nothing -> insert (HoodleDocRoot True (T.pack hdir)) >> return ()
      Just root -> update (entityKey root) [ HoodleDocRootLoc =. (T.pack hdir) ] >> return ()
    dumpTable "hoodle_doc_root"

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


data HoodleFile = HoodleFile { hoodleFileUuid :: T.Text
                             , hoodleFileOwner :: T.Text 
                             , hoodleFilePath :: T.Text }
                deriving (Show)

{- 
instance ToJSON HoodleFile where 
    toJSON HoodleFile {..} = object [ "uuid"     .= toJSON hoodleFileUuid
                                    , "owner"    .= toJSON hoodleFileOwner
                                    , "filepath" .= toJSON hoodleFilePath 
                                    ] 
-}

instance FromJSON HoodleFile where 
    parseJSON (Object v) = HoodleFile <$> v .: "uuid" 
                                      <*> v .: "owner"
                                      <*> v .: "filepath"
    parseJSON _ = mzero 

dbdiffwork :: FilePath -> IO ()
dbdiffwork newdbfile = do 
  homedir <- getHomeDirectory 
  newdbstr <- readFile newdbfile
  olddbjson <- L.hGetContents stdin
  let molddb :: Maybe [HoodleFile] = decode olddbjson  -- parseEither parseJSON olddbstr
  case molddb of
    Nothing -> return ()
    Just olddb -> do 
      let f HoodleFile {..} = (T.unpack hoodleFileUuid ++ " " ++ "fda122ccca11d29a8bd9acbcd714eeef" ++ " " ++ show hoodleFilePath)
          olddbstr = DL.intercalate "\n" (map f olddb)
          makedb = M.fromList . map TextFileDB.splitfunc . lines 
          (newdbmap,olddbmap) = (makedb newdbstr, makedb olddbstr) 
      (L.putStrLn . encodePretty) (checkdiff olddbmap newdbmap)
  

  -- let newdbfile = homedir </> "Dropbox" </> "hoodleiddb.dat"
      -- for the time being
      -- olddbfile = homedir </> "Dropbox" </> "hoodleiddb.dat.old"
   
  -- newdbstr <- readFile newdbfile 
  -- for the time being
  -- olddbstr <- readFile olddbfile
  -- let olddbstr = ""

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

dbgetwork :: String -> String -> String -> IO ()
dbgetwork url idee pwd = do
    hubwork url idee pwd $ \ck -> do 
        request'' <- N.parseUrl (url <> "/dumpdb")
        let requesttask = request'' 
              { N.method = "GET"
              , N.requestHeaders = ("Content-Type", "application/json") 
                                   : ("Cookie",ck) 
                                   : N.requestHeaders request''
              }
        runResourceT $ N.withManager $ \manager -> do  
          response <- N.http requesttask manager
          content <- N.responseBody response $$+- CL.consume 
          liftIO $ B.putStrLn (mconcat content)
    return ()
   
main :: IO () 
main = do 
  params <- cmdArgs mode
  case params of
    List                -> listwork
    Info uuid           -> infowork uuid
    SingleFile mhdir fp -> singlefilework mhdir fp 
    AllFiles mhdir      -> allfilework mhdir
    ChangeRoot hdir     -> changerootwork hdir
    DBSync url idee pw  -> dbsyncwork url idee pw
    DBDiff fp           -> dbdiffwork fp
    DBGet url idee pw   -> dbgetwork url idee pw  

    {- 
    DBMigrateToSqlite  -> dbmigrate2sqlite 
    -}

