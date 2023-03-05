{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Janus.Utils.DB (runDB) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Database.Persist.Postgresql

runDB :: (MonadIO m, MonadUnliftIO m) => ConnectionPool -> SqlPersistT m a -> m a
runDB pool query = runSqlPool query pool

