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
    deriving Show 
|]