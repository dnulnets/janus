{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad.Logger       (LoggingT, runStderrLoggingT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Database.Persist           (PersistStoreWrite (insert))
import           Database.Persist.Sql       (SqlBackend)
import           Database.Persist.Sqlite    (ConnectionPool, runMigration,
                                             runSqlPool, withSqlitePool)
import           Janus                      (waiapp)
import qualified Janus.Data.Config          as C
import           Janus.Data.Model           (User (User), migrateAll)
import           Janus.Settings             (Settings (Settings, config, dbpool))
import qualified Login
import qualified Static
import           Test.Hspec                 (Spec, hspec)
import           Test.Hspec.Wai             (liftIO, with)

-- | The default configuraion used for testing
conf :: C.Config
conf = C.Config (C.Token "testissuer" "testkey" 600) (C.Database ":memory:" 5)

-- | Main startup
main :: IO ()
main = startup

-- | Sets up the database and run the tests
startup :: IO ()
startup = do
  runStderrLoggingT $ withSqlitePool (C.url (C.database conf)) (C.size (C.database conf)) $ \pool -> do
    _ <- runSqlPool (runMigration migrateAll) pool
    _ <- runSqlPool setup pool
    liftIO $ hspec $ spec pool

-- | Preload the database with some test data
setup :: ReaderT SqlBackend (LoggingT IO) ()
setup = do
    _ <- insert $ User "test1" "bf3cfe1f-8dea-4c08-aa38-49d3098fce1e" "$2y$10$Cwk1ulIHlkKy0pofcUBaVerTUEksxbFX9pLJeobN8uzEftVj3Zyra" "test1@test.home.local"
    _ <- insert $ User "test2" "bf3cfe1f-8dea-4c08-aa38-49d3098fce1f" "$2y$10$wiZf6SMsdH/ie.0uIkfm1ur1VM97RjJIBHquMz30z5M5ua6qCnFYy" "test2@test.home.local"
    return ()

-- | Sum of all specifications
spec :: ConnectionPool -> Spec
spec pool = with (waiapp $ Settings {config = conf, dbpool = pool}) $ do
  Static.spec
  Login.spec

