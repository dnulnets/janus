{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
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
module Janus.Utils.DB (runDB, textToKey, keyToText) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Char8 (unpack)
import Database.Persist.Postgresql (ConnectionPool, SqlPersistT, runSqlPool)
import Database.Persist.Sql (toSqlKey, fromSqlKey, Key, ToBackendKey, SqlBackend)
import Control.Monad.Reader (MonadReader(ask))
import Janus.Settings (Settings(..))
import Control.Monad.Trans (lift, liftIO, MonadIO)

-- | Convert from Text to database key
textToKey :: ToBackendKey SqlBackend record => Text -> Key record
textToKey key = toSqlKey $ read $ unpack $ encodeUtf8 key

-- | Convert from Text to database key
keyToText :: ToBackendKey SqlBackend record => Key record -> Text
keyToText key = pack $ show $ fromSqlKey key

-- | Runs an sql query and returns with the result
runDB::(MonadReader Settings m, MonadIO m) => ReaderT SqlBackend IO b -> m b
runDB query = do
    settings <- ask
    liftIO $ runSqlPool query (dbpool settings)

