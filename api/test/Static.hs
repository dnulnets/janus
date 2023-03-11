{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Static (spec) where

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

-- | Test that the static files are served
spec :: SpecWith (Arg (WaiExpectation st))
spec = describe "Static files" $ do
    it "favicon responds with 200" $ do
      get "/static/favicon.ico" `shouldRespondWith` 200
    it "index.html responds with 200" $ do
      get "/" `shouldRespondWith` 200
    it "index.js responds with 200" $ do
      get "/index.js" `shouldRespondWith` 200
