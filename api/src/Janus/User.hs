{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

-- |
-- Module      : Janus.Static
-- Description : The static part of the Janus application.
-- Copyright   : (c) Tomas Stenlund, 2023
-- License     : GNU AFFERO GENERAL PUBLIC LICENSE
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains the static part of the application that servers static pages.
module Janus.User (app, LoginResponse(..), LoginRequest(..)) where

import           Control.Applicative       (Alternative (empty))
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Reader      (ask)
import           Control.Monad.Trans       (lift, liftIO)
import           Data.Aeson                (FromJSON (parseJSON),
                                            KeyValue ((.=)), ToJSON (toJSON),
                                            Value (Object), object, (.:))
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (Text)
import           Data.Time.Clock.System    (SystemTime (systemSeconds),
                                            getSystemTime)
import           Data.UUID                 (UUID)
import qualified Database.Persist.Sql      as DB
import           Janus.Core                (JScottyM)
import qualified Janus.Data.Config         as C
import Janus.Data.Role
import           Janus.Data.Model          (Unique (UniqueUserUsername),
                                            User (userActive, userEmail, userGuid, userPassword, userUsername))
import           Janus.Settings            (Settings (config))
import           Janus.Utils.DB            (runDB)
import           Janus.Utils.JWT           (createToken)
import           Janus.Utils.Password      (authValidatePassword)
import           Network.HTTP.Types.Status (unauthorized401)
import           Web.Scotty.Trans          (get, json, jsonData, post, status,
                                            text)
import Janus.Utils.Auth

-- | User information for the login response
data LoginResponse = LoginResponse
  { guid     :: UUID,
    username :: Text,
    email    :: Text,
    active   :: Bool,
    token    :: Text
  }
  deriving (Show)

-- | User information for the login response
data LoginRequest = LoginRequest
  { qusername :: Text,
    qpassword :: Text
  }
  deriving (Show)

instance ToJSON LoginResponse where
  -- this generates a Value
  toJSON (LoginResponse _guid _username _email _active _token) =
    object ["user" .= object ["guid" .= _guid, "username" .= _username, "email" .= _email, "token" .= _token, "active" .= _active]]

instance FromJSON LoginResponse where
  parseJSON (Object v) = do
    w <- v .: "user"
    LoginResponse
      <$> w
      .: "guid"
      <*> w
      .: "username"
      <*> w
      .: "email"
      <*> w
      .: "token"
      <*> w
      .: "active"
  parseJSON _ = empty


instance FromJSON LoginRequest where
  parseJSON (Object v) = do
    w <- v .: "user"
    LoginRequest
      <$> w
      .: "username"
      <*> w
      .: "password"
  parseJSON _ = empty

-- | The part of the application that serve user specific functions.
-- |
-- | The following url's are handled:
-- |
-- | POST /api/user/login : Check the user and password, return with a token and user if valid and active
-- | GET  /api/user/login : Check that the authorization header contains a valid token and that the user is valid and active, returns with the user
-- |
app :: (MonadIO m) => JScottyM m ()
app = do

  -- |Handles a login request and returns with the token if the user is valid and active
  post "/api/user/login" $ do
    req <- jsonData
    settings <- lift ask
    dbuser <- runDB $ DB.getBy $ UniqueUserUsername (qusername req)
    case dbuser of
      Just (DB.Entity _ user) | userActive user && authValidatePassword (userPassword user) (qpassword req) -> do
        seconds <- liftIO $ fromIntegral . systemSeconds <$> getSystemTime
        let jwt = createToken (C.key (C.token (config settings))) seconds (C.valid (C.token (config settings))) (C.issuer (C.token (config settings))) (userGuid user)
        let userResponse = LoginResponse {guid = (userGuid user), username = (userUsername user), email = (userEmail user),
          active = (userActive user), token = jwt}
        json userResponse
      _ -> status unauthorized401

  -- |Checks a token and returns with the user if the user is valid and active
  get "/api/user/login" $ do

    user <- getAuthenticated
    token <- getToken
    case user of
      Just u -> do
        let userResponse = LoginResponse {guid = (userGuid u), username = (userUsername u),
          email = (userEmail u), active = (userActive u), token = fromMaybe "" token}
        json userResponse
      Nothing -> status unauthorized401

  get "/api/tomas" $ do
    roleRequired [Administrator]
    text "Hejsan svejsan"
