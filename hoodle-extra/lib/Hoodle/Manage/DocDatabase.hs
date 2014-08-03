{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Hoodle.Manage.DocDatabase where

import Data.Text
import Database.Esqueleto
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

-- 

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
HoodleDocLocation
    fileid  Text
    filemd5 Text
    fileloc Text 
    FileIDKey fileid
    deriving Show 
|]

share [mkPersist sqlSettings, mkMigrate "migrateDocRoot"] [persistLowerCase|
HoodleDocRoot 
    onoff Bool
    loc   Text
    UniqueDocRoot onoff
    deriving Show
|]



