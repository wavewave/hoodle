{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Hoodle.Manage where

-- import Control.Monad.IO.Class (MonadIO(..))
-- import Control.Monad.Logger (MonadLogger(..))
-- import Control.Monad.Trans.Control (MonadBaseControl(..))
-- import Control.Monad.Trans.Resource (MonadResource(..))
-- import Data.Conduit (($$))
-- import Data.Conduit.List as CL
import Data.Text (Text)
-- import Database.Esqueleto
-- import Database.Persist.Sqlite (runSqlite, runMigrationSilent)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
-- import Database.Persist.Sql (rawQuery)
-- 
import Hoodle.Manage.Type.MD5
import Hoodle.Manage.Type.UUID

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
  HoodleFile 
    uuid     UUID
    md5hash  MD5 
    location Text
    deriving Show
|]


