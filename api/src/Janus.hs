{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
-- |
-- Module      : Janus
-- Description : The concatenated parts of the Janus application.
-- Copyright   : (c) Tomas Stenlund, 2023
-- License     : GNU
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module concats all part of the application.
module Janus where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Text.Encoding (encodeUtf8)
import Database.Persist.Postgresql
import Database.Persist.Sql
import Janus.Core (JScottyM)
import qualified Janus.Data.Config as C
import Janus.Data.Model
import Janus.Settings (Settings (..))
import qualified Janus.Static as JS
import qualified Janus.User as JU
import Janus.Utils.DB
import Network.Wai (Application)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Web.Scotty.Trans as T (middleware, scottyAppT, scottyT)

app :: (MonadIO m) => JScottyM m ()
app = middleware logStdoutDev <> JS.app <> JU.app

waiapp :: Settings -> IO Application
waiapp s = scottyAppT (`runReaderT` s) app

-- | Run the application
runApp :: (MonadIO m, MonadLogger m) => Settings -> m ()
runApp s = do
  jscotty s app
  where
    jscotty s = scottyT 8080 (`runReaderT` s)

startup :: IO ()
startup = do
  putStrLn "JANUS: Reading config"
  conf <- C.readConfig "./conf.yaml"
  runStderrLoggingT $ withPostgresqlPool (encodeUtf8 (C.url (C.database conf))) (C.size (C.database conf)) $ \pool -> do
    logInfoN "JANUS: Migrating database"
    runDB pool $ runMigration migrateAll

    -- Run the application
    logInfoN "JANUS: Starting server"
    runApp $ Settings {config = conf, dbpool = pool}