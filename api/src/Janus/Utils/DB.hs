{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Janus.Utils.DB
-- Description : Generic database functionality
-- Copyright   : (c) Tomas Stenlund, 2023
-- License     : GNU AFFERO GENERAL PUBLIC LICENSE
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Functionality for various database functions that are generic for the entire application.
module Janus.Utils.DB (runDB) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Database.Persist.Postgresql (ConnectionPool, SqlPersistT, runSqlPool)

-- | Runs an sql query and returns with the result
runDB ::
  (MonadUnliftIO m) =>
  -- | The connection pool
  ConnectionPool ->
  -- | The query
  SqlPersistT m a ->
  -- | The result of the query
  m a
runDB pool query = runSqlPool query pool
