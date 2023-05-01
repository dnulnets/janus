{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

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

import           Control.Monad.Reader        (MonadReader (ask))
import           Control.Monad.Trans         (MonadIO, liftIO)
import           Control.Monad.Trans.Reader  (ReaderT)
import           Database.Persist.Postgresql (runSqlPool)
import           Database.Persist.Sql        (SqlBackend)
import           Janus.Settings              (Settings (..))

-- | Runs an sql query and returns with the result
runDB::(MonadReader Settings m, MonadIO m) => ReaderT SqlBackend IO b -> m b
runDB query = do
    settings <- ask
    liftIO $ runSqlPool query (dbpool settings)

