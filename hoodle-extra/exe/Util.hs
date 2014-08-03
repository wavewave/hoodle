{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Util where

import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import           Data.Conduit (($$))
import qualified Data.Conduit.List as CL 
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Database.Persist.Sqlite
import           Database.Persist.Sql (rawQuery)

dumpTable :: (MonadBaseControl IO m, MonadIO m, MonadThrow m) => 
             T.Text -> SqlPersistT (NoLoggingT (ResourceT m)) ()
dumpTable tablename = rawQuery ("select * from " <> tablename) [] $$ CL.mapM_ (liftIO . print)
