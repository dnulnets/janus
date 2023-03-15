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

import           Control.Applicative             (Alternative (empty))
import           Control.Monad                   (join)
import           Control.Monad.IO.Class          (MonadIO)
import           Control.Monad.Reader            (ask)
import           Control.Monad.Trans             (lift, liftIO)
import           Data.Aeson                      (FromJSON (parseJSON),
                                                  KeyValue ((.=)),
                                                  ToJSON (toJSON),
                                                  Value (Object), object, (.:))
import           Data.Text                       (Text)
import           Data.Text.Encoding              (decodeUtf8', encodeUtf8)
import           Data.Text.Lazy                  (toStrict)
import           Data.Time.Clock.System          (SystemTime (systemSeconds),
                                                  getSystemTime)
import qualified Database.Persist.Sql            as DB
import           Janus.Core                      (JScottyM)
import qualified Janus.Data.Config               as C
import           Janus.Data.Model
import           Janus.Settings
import           Janus.Utils.DB
import           Janus.Utils.JWT                 (createToken, getSubject)
import           Janus.Utils.Password
import           Network.HTTP.Types.Status
import           Network.Wai.Middleware.HttpAuth (extractBearerAuth)
import           Web.Scotty.Trans                (get, header, json, jsonData,
                                                  post, status)

-- | User information for the login response
data LoginResponse = LoginResponse
  { guid      :: Text,
    username :: Text,
    email    :: Text,
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
  toJSON (LoginResponse _guid _username _email _token) =
    object ["user" .= object ["guid" .= _guid, "username" .= _username, "email" .= _email, "token" .= _token]]

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

-- | The part of the application that serve user specific functions
app :: (MonadIO m) => JScottyM m ()
app = do

  post "/api/user/login" $ do
    req <- jsonData
    settings <- lift ask
    dbuser <- runDB $ DB.getBy $ UniqueUserUsername (qusername req)
    case dbuser of
      Just (DB.Entity _ user) | authValidatePassword (userPassword user) (qpassword req) -> do
        seconds <- liftIO $ fromIntegral . systemSeconds <$> getSystemTime
        let jwt = createToken (C.key (C.token (config settings))) seconds (C.valid (C.token (config settings))) (C.issuer (C.token (config settings))) (userGuid user)
        let userResponse = LoginResponse {guid = (userGuid user), username = (userUsername user), email = (userEmail user), token = jwt}
        json userResponse
      _ -> status unauthorized401

  get "/api/user" $ do
    settings <- lift ask
    auth <- header "Authorization"
    let bearer = extractBearerAuth . encodeUtf8 . toStrict <$> auth
    case decodeUtf8' <$> join bearer of
      Just (Right b) -> do
        seconds <- liftIO $ fromIntegral . systemSeconds <$> getSystemTime
        case getSubject (C.key (C.token (config settings))) seconds b (C.issuer (C.token (config settings))) of
          Just u -> do
            dbuser <- runDB $ DB.getBy $ UniqueUserGUID u
            case dbuser of
              Just (DB.Entity _ user) -> do
                let userResponse = LoginResponse {guid = (userGuid user), username = (userUsername user), email = (userEmail user), token = b}
                json userResponse
              Nothing -> status unauthorized401
          Nothing -> status unauthorized401
      _ -> status unauthorized401
