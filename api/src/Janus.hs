{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Janus
-- Description : Application startup
-- Copyright   : (c) Tomas Stenlund, 2023
-- License     : GNU AFFERO GENERAL PUBLIC LICENSE
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Concatenates the different parts of the application and starts up the database and server.
--
module Janus (startup, waiapp) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger (logInfoN, runStderrLoggingT)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Text.Encoding (encodeUtf8)
import Database.Persist.Postgresql (runMigration, withPostgresqlPool, runSqlPool)
import Janus.Core (JScottyM)
import qualified Janus.Data.Config as C
import Janus.Data.Model (migrateAll)
import Janus.Settings (Settings (..))
import qualified Janus.Static as JS
import qualified Janus.User as JU
import Janus.Utils.DB (runDB)
import Network.Wai (Application)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Web.Scotty.Trans as T (middleware, scottyAppT, scottyT)

-- | The concatenated application
app :: (MonadIO m) => JScottyM m ()
app = middleware logStdoutDev <> JS.app <> JU.app

-- | The application that can be used for the testbed
waiapp ::
  -- | The application settings
  Settings ->
  IO Application
waiapp s = scottyAppT (`runReaderT` s) app

-- | Run the application
runApp :: (MonadIO m) =>
  -- | The applications settings
  Settings ->
  m ()
runApp s = scottyT 8080 (`runReaderT` s) app

-- | The bootstrap of the application.
startup :: IO ()
startup = do
  putStrLn "JANUS: Reading config"
  conf <- C.readConfig "./conf.yaml"
  runStderrLoggingT $ withPostgresqlPool (encodeUtf8 (C.url (C.database conf))) (C.size (C.database conf)) $ \pool -> do
    logInfoN "JANUS: Migrating database"
    runSqlPool (runMigration migrateAll) pool

    -- Run the application
    logInfoN "JANUS: Starting server"
    runApp $ Settings {config = conf, dbpool = pool}