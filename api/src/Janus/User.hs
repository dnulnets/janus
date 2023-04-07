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
module Janus.User (app, UserResponse(..), LoginRequest(..)) where

import           Control.Applicative       (Alternative (empty))
import           Control.Monad.Catch
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Reader      (ask)
import           Control.Monad.Trans       (lift, liftIO)
import           Data.Aeson                (FromJSON (parseJSON),
                                            KeyValue ((.=)), ToJSON (toJSON),
                                            Value (Object), object, (.:), (.:?))
import           Data.Text                 (Text)
import           Data.Time.Clock.System    (SystemTime (systemSeconds),
                                            getSystemTime)
import           Data.UUID                 (fromString)
import qualified Database.Persist.Sql      as DB
import           Janus.Core                (JScottyM)
import qualified Janus.Data.Config         as C
import           Janus.Data.Model          (EntityField (..), Key (UserKey),
                                            Unique (UniqueUserUsername),
                                            User (User, userActive, userEmail, userGuid, userPassword, userUsername))
import qualified Janus.Data.Role           as R
import           Janus.Data.UUID
import           Janus.Settings            (Settings (config))
import           Janus.Utils.Auth
import           Janus.Utils.DB            (runDB)
import           Janus.Utils.JWT           (createToken)
import           Janus.Utils.Password      (authHashPassword,
                                            authValidatePassword)
import           Network.HTTP.Types.Status (created201, internalServerError500,
                                            notFound404, ok200, unauthorized401, badRequest400)
import           Web.Scotty.Trans          (delete, get, json, jsonData, param,
                                            post, put, status, rescue)

-- | User information for the login response
data UserResponse = UserResponse
  { guid     :: UUID,
    username :: Text,
    email    :: Text,
    active   :: Bool,
    token    :: Maybe Text,
    password :: Maybe Text
  }
  deriving (Show)

-- | The request for login
data LoginRequest = LoginRequest
  { lrusername :: Text,
    lrpassword :: Text
  }
  deriving (Show)

-- | The request for creating the user
data CreateUserRequest = CreateUserRequest
  { curguid     :: UUID,
    curusername :: Text,
    curemail    :: Text,
    curactive   :: Bool,
    curpassword :: Text
  }
  deriving (Show)

-- | The request for creating the user
data UpdateUserRequest = UpdateUserRequest
  { uurguid     :: UUID,
    uurusername :: Text,
    uuremail    :: Text,
    uuractive   :: Bool,
    uurpassword :: Maybe Text
  }
  deriving (Show)

instance ToJSON UserResponse where
  -- this generates a Value
  toJSON (UserResponse _guid _username _email _active _token _password) =
    object ["user" .= object ["guid" .= _guid, "username" .= _username, "email" .= _email,
      "token" .= _token, "active" .= _active, "password" .= _password]]

instance FromJSON UserResponse where
  parseJSON (Object v) = do
    w <- v .: "user"
    UserResponse
      <$> w
      .: "guid"
      <*> w
      .: "username"
      <*> w
      .: "email"
      <*> w
      .: "active"
      <*> w
      .:? "token"
      <*> w
      .:? "password"
  parseJSON _ = empty

instance FromJSON CreateUserRequest where
  parseJSON (Object v) = do
    w <- v .: "user"
    CreateUserRequest
      <$> w
      .: "guid"
      <*> w
      .: "username"
      <*> w
      .: "email"
      <*> w
      .: "active"
      <*> w
      .: "password"
  parseJSON _ = empty

instance FromJSON UpdateUserRequest where
  parseJSON (Object v) = do
    w <- v .: "user"
    UpdateUserRequest
      <$> w
      .: "guid"
      <*> w
      .: "username"
      <*> w
      .: "email"
      <*> w
      .: "active"
      <*> w
      .:? "password"
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
-- | GET  /api/user/<uuid> : Retrieves a specified user
-- | POST /api/user       : Updates a user
-- | PUT  /api/user       : Creates a user
-- | DELETE /api/user     : Deletes a specified user
-- |
app :: (MonadIO m, MonadCatch m) => JScottyM m ()
app = do

  -- |Handles a login request and returns with the token if the user is valid and active
  post "/api/login" $ do
    req <- jsonData
    settings <- lift ask
    dbuser <- runDB $ DB.getBy $ UniqueUserUsername (lrusername req)
    case dbuser of
      Just (DB.Entity _ user) | userActive user && authValidatePassword (userPassword user) (lrpassword req) -> do
        seconds <- liftIO $ fromIntegral . systemSeconds <$> getSystemTime
        let jwt = createToken
              ((C.key . C.token . config) settings) 
              seconds
              ((C.valid . C.token . config) settings)
              ((C.issuer . C.token . config) settings)
              (userGuid user)
        let userResponse = UserResponse {guid = (userGuid user), username = (userUsername user), email = (userEmail user),
          active = (userActive user), token = Just jwt, password = Nothing}
        json userResponse
      _ -> status unauthorized401

  -- |Checks a token and returns with the user if the user is valid and active
  get "/api/login" $ do

    user <- getAuthenticated
    t <- getToken
    case user of
      Just u -> do
        let userResponse = UserResponse {guid = (userGuid u), username = (userUsername u),
          email = (userEmail u), active = (userActive u), token = t, password = Nothing}
        json userResponse
      Nothing -> status unauthorized401

  -- |Returns with the use based on the key
  get "/api/user/:uuid" $ do
    roleRequired [R.Administrator]
    uuid <- fromString <$> param "uuid"
    case uuid of
      Just k -> do
        dbuser <- runDB $ DB.get $ UserKey k
        case dbuser of
          Just u -> do
            let userResponse = UserResponse {guid = (userGuid u), username = (userUsername u),
              email = (userEmail u), active = (userActive u), token = Nothing, password = Nothing}
            json userResponse
          Nothing -> status notFound404
      Nothing -> status badRequest400

  -- |Creates a user
  put "/api/user" $ (do
    roleRequired [R.Administrator]
    req <- jsonData
    settings <- lift ask
    pwd <- liftIO $ authHashPassword ((C.cost . C.password . config) settings) (curpassword req)
    _ <- runDB $ DB.insert $ User { userUsername = (curusername req), userGuid = (curguid req), userPassword = pwd,
      userEmail = (curemail req), userActive = (curactive req)}
    status created201) `catch`
      (\(SomeException e) -> do
        status internalServerError500
        liftIO $ print e)

  -- |Updates a user
  post "/api/user/:uuid" $ ( do
    roleRequired [R.Administrator]
    req <- jsonData
    settings <- lift ask
    uuid <- fromString <$> param "uuid"
    case uuid of
      Just k -> do
        pwd <- liftIO $ mapM (authHashPassword ((C.cost . C.password . config) settings)) (uurpassword req)
        runDB $ DB.update (UserKey k) $ [ UserUsername DB.=. uurusername req, UserGuid DB.=. uurguid req,
          UserEmail DB.=. uuremail req, UserActive DB.=. uuractive req] <> (maybe [] (\i->[UserPassword DB.=. i]) pwd)
        status ok200
      Nothing -> status badRequest400) `catch`
      (\(SomeException e) -> do
        status internalServerError500
        liftIO $ print e)

  -- |Delete a user
  delete "/api/user/:uuid" $ ( do
    roleRequired [R.Administrator]
    uuid <- fromString <$> param "uuid"
    case uuid of
      Just k -> do
        runDB $ DB.delete $ UserKey k
        status ok200
      Nothing -> status badRequest400) `catch`
      (\(SomeException e) -> do
        status internalServerError500
        liftIO $ print e)

  -- |Returns with a list of users
  get "/api/users" $ do
    roleRequired [R.Administrator]
    settings <- lift ask
    start <- param "start" `rescue` (\_ -> pure 0)
    nof <- param "n" `rescue` (\_ -> pure $ fromIntegral $ (C.length . C.ui . config) settings)
    dbl <- runDB $ DB.selectList [] [DB.LimitTo nof, DB.OffsetBy start, DB.Asc UserUsername]
    json $ map prepare dbl
    where
      prepare::DB.Entity User -> UserResponse
      prepare (DB.Entity _ u) = UserResponse {guid = (userGuid u), username = (userUsername u),
              email = (userEmail u), active = (userActive u), token = Nothing, password = Nothing}
