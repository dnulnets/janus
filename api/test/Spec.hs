{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Monad.Logger (logInfoN, runStderrLoggingT)
import Data.Text.Encoding (encodeUtf8)
import Database.Persist
import Database.Persist.Sqlite (ConnectionPool, runMigration, runSqlPool, withSqlitePool)
import Janus (waiapp)
import Janus.Data.Config (Config (..))
import qualified Janus.Data.Config as C
import Janus.Data.Model
import Janus.Settings
import Network.HTTP.Types.Header
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import qualified Static as Static
import qualified Login as Login

-- | The default configuraion used for testing
conf = C.Config (C.Token "testissuer" "testkey" 600) (C.Database ":memory:" 5)

-- | Main startup
main :: IO ()
main = startup

-- | Sets up the database and run the tests
startup :: IO ()
startup = do
  runStderrLoggingT $ withSqlitePool (C.url (C.database conf)) (C.size (C.database conf)) $ \pool -> do
    runSqlPool (runMigration migrateAll) pool
    runSqlPool setup pool
    liftIO $ hspec $ spec pool

setup = do
    insert $ User "test1" "bf3cfe1f-8dea-4c08-aa38-49d3098fce1e" "$2y$10$Cwk1ulIHlkKy0pofcUBaVerTUEksxbFX9pLJeobN8uzEftVj3Zyra" "test1@test.home.local"
    insert $ User "test2" "bf3cfe1f-8dea-4c08-aa38-49d3098fce1f" "$2y$10$wiZf6SMsdH/ie.0uIkfm1ur1VM97RjJIBHquMz30z5M5ua6qCnFYy" "test2@test.home.local"

-- | Sum of all specifications
spec :: ConnectionPool -> Spec
spec pool = with (waiapp $ Settings {config = conf, dbpool = pool}) $ do
  Static.spec
  Login.spec

