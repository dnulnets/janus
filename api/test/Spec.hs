{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Monad.Logger (logInfoN, runStderrLoggingT)
import Data.Text.Encoding (encodeUtf8)
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
    liftIO $ hspec $ spec pool

-- | Sum of all specifications
spec :: ConnectionPool -> Spec
spec pool = with (waiapp $ Settings {config = conf, dbpool = pool}) $ do
  Static.spec

