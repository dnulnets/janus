{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Login (spec) where

import Control.Monad.Logger (logInfoN, runStderrLoggingT)
import Data.Aeson (decode)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.System
  ( SystemTime (systemSeconds),
    getSystemTime,
  )
import Database.Persist.Sqlite (ConnectionPool, runMigration, runSqlPool, withSqlitePool)
import Janus (waiapp)
import qualified Janus.Data.Config as C
import Janus.Data.Model
import Janus.Settings
import Janus.User
import Janus.Utils.JWT
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

-- | Test that the static files are served
spec :: SpecWith (Arg (WaiExpectation st))
spec = describe "Login" $ do
  it "Refresh user from token but do not include any token, responds with 401" $ do
    get "/api/user/refresh" `shouldRespondWith` 401

  it "Refresh user from faulty token, responds with 401" $ do
    shouldRespondWith
      ( request
          methodGet
          "/api/user/refresh"
          [(hAuthorization, "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJleHAiOjE2Nzg0NjMwNzEsImlhdCI6MTY3ODQ2MTI3MSwiaXNzIjoiaHR0cHM6Ly9zdGVubHVuZC5ldSIsInN1YiI6ImE5ZjdiOTk0LTM1YzItNDRlNS1hYTA1LTYxMDRkMGVlZGIzZCJ9.FuSShCBsOOOaDdHlioF3a8jdfaOz0oW_jo5O1b_NIwE")]
          ""
      )
      401

  it "Refresh user from a correct token, responds with 200 and user info" $ do
    seconds <- liftIO $ fromIntegral . systemSeconds <$> getSystemTime
    let jwt = encodeUtf8 (createToken "testkey" seconds 600 "testissuer" "bf3cfe1f-8dea-4c08-aa38-49d3098fce1e")
    request methodGet "/api/user/refresh" [(hAuthorization, "Bearer " <> jwt)] "" `shouldRespondWith` matcher seconds

  it "Login a real user with faulty password, respond with 401" $ do
    post "/api/user/login" [json|{"user":{"password":"wrong","username":"test1"}}|] `shouldRespondWith` 401

  it "Login a fake user, respond with 401" $ do
    post "/api/user/login" [json|{"user":{"password":"test1","username":"grgrfaqefager"}}|] `shouldRespondWith` 401

  it "Login a real user and correct password, respond with 200, user info and token" $ do
    seconds <- liftIO $ fromIntegral . systemSeconds <$> getSystemTime
    post "/api/user/login" [json|{"user":{"password":"test1","username":"test1"}}|] `shouldRespondWith` (matcher seconds)

  where
    
    matcher s =
      ResponseMatcher
        { matchStatus = 200,
          matchHeaders = [],
          matchBody = MatchBody $ check s
        }

    check s _ body = case decode body of
      Just lr -> if expected s lr then Nothing else Just ("Wrong response: " <> show lr)
      _ -> Just "Unable to parse json"

    expected s lr = (username lr) == "test1" && 
      (uid lr) == "bf3cfe1f-8dea-4c08-aa38-49d3098fce1e" && 
      (email lr) == "test1@test.home.local" && 
      (maybe False ((==) "bf3cfe1f-8dea-4c08-aa38-49d3098fce1e") $ getSubject "testkey" s (token lr) "testissuer")
