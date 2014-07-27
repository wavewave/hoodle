{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Util where

import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import           Data.Conduit (($$))
import qualified Data.Conduit.List as CL 
import           Database.Persist.Sqlite
import           Database.Persist.Sql (rawQuery)

dumpTable :: (MonadBaseControl IO m, MonadIO m, MonadThrow m) => SqlPersistT (NoLoggingT (ResourceT m)) ()
dumpTable = rawQuery "select * from hoodle_doc_location" [] $$ CL.mapM_ (liftIO . print)
