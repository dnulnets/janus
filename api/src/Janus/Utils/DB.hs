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
module Janus.Utils.DB (runDB, textToKey, keyToText) where

import           Control.Monad.Reader        (MonadReader (ask))
import           Control.Monad.Trans         (MonadIO, liftIO)
import           Control.Monad.Trans.Reader  (ReaderT)
import           Data.ByteString.Char8       (unpack)
import           Data.Text                   (Text, pack)
import           Data.Text.Encoding          (encodeUtf8)
import           Database.Persist.Postgresql (runSqlPool)
import           Database.Persist.Sql        (Key, SqlBackend, ToBackendKey,
                                              fromSqlKey, toSqlKey)
import           Janus.Settings              (Settings (..))

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


