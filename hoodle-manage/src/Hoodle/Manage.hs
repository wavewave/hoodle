{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts, EmptyDataDecls #-}

module Hoodle.Manage where

import Control.Monad.IO.Class (liftIO)
import Data.Conduit (($$))
import Data.Conduit.List as CL
import Data.Text (Text)
import Database.Esqueleto
-- import Database.Persist
import Database.Persist.Sqlite (runSqlite, runMigrationSilent)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import Database.Persist.Sql (rawQuery)

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
  Author 
    name   Text
    email  Text
    EmailKey email
    deriving Show
  Tutorial
    title  Text
    url    Text
    school Bool
    author AuthorId
    deriving Show
|]

-- sqlite

maintest :: IO () 
maintest = 
  runSqlite ":memory:" $ do 
         buildDB
         liftIO (putStrLn "dump tutorial")
         dumpTutorial
         liftIO (putStrLn "dump author")
         dumpAuthor
         liftIO (putStrLn "test esqueleto")
         tuts <- select $ from $ \(a,t) -> do
                   where_ ( a ^. AuthorEmail ==. val "ann@example.com" &&. t ^. TutorialAuthor ==. a ^. AuthorId)
                   return t
         liftIO $ print tuts
                 
buildDB = do
    runMigration migrateTables
    school <- insert $ Author "School of Haskell" "school@example.com"
    anne <- insert $ Author "Ann Author" "ann@example.com"
    insert $ Tutorial "Basic Haskell" "https://fpcomplete.com/school/basic" True school
    insert $ Tutorial "A monad tutorial" "https://test.com" False anne

dumpTutorial = rawQuery "select * from Tutorial" [] $$ CL.mapM_ (liftIO . print)

dumpAuthor = rawQuery "select * from Author" [] $$ CL.mapM_ (liftIO . print)

