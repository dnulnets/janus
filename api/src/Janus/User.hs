{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use mapM_" #-}

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

import           Control.Applicative        (Alternative (empty))
import           Control.Monad.Catch        (MonadCatch (..),
                                             SomeException (SomeException))
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Reader       (ask)
import           Control.Monad.Trans        (lift, liftIO)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Aeson                 (FromJSON (parseJSON),
                                             KeyValue ((.=)), ToJSON (toJSON),
                                             Value (Object), object, (.:),
                                             (.:?))
import           Data.Maybe                 (fromJust, fromMaybe, isJust,
                                             isNothing, listToMaybe)
import           Data.Text                  (Text)
import           Data.Text.Lazy             (pack)
import           Data.Time.Clock.System     (SystemTime (systemSeconds),
                                             getSystemTime)
import           Data.UUID                  (fromString, toString)
import qualified Database.Persist.Sql       as DB
import           Janus.Core                 (JScottyM)
import qualified Janus.Data.Config          as C
import           Janus.Data.Message         (Message (..), JanusError (..))
import           Janus.Data.Model           (AssignedRole (AssignedRole, assignedRoleType, assignedRoleUser),
                                             EntityField (..),
                                             Key (AssignedRoleKey, UserKey, unAssignedRoleKey),
                                             Unique (UniqueUserUsername),
                                             User (User, userActive, userEmail, userPassword, userUsername))
import qualified Janus.Data.Role            as R
import           Janus.Data.User            (nofUsers)
import           Janus.Data.UUID            (UUID)
import           Janus.Settings             (Settings (config))
import           Janus.Utils.Auth           (getAuthenticated, getToken,
                                             roleRequired)
import           Janus.Utils.DB             (runDB)
import           Janus.Utils.JWT            (createToken)
import           Janus.Utils.Password       (authHashPassword,
                                             authValidatePassword)
import           Network.HTTP.Types.Status  (badRequest400, conflict409,
                                             created201, internalServerError500,
                                             notFound404, ok200,
                                             unauthorized401)
import           Web.Scotty.Trans           (delete, get, json, jsonData, param,
                                             post, put, rescue, status, text)

import qualified Data.Set                   as S

-- | User information for the login response
data UserResponse = UserResponse
  { key      :: UUID,
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
  { curusername :: Text,
    curemail    :: Text,
    curactive   :: Bool,
    curpassword :: Text
  }
  deriving (Show)

-- | The request for creating the user
data UpdateUserRequest = UpdateUserRequest
  { uurusername :: Text,
    uuremail    :: Text,
    uuractive   :: Bool,
    uurpassword :: Maybe Text
  }
  deriving (Show)

-- | The response for retrieveing the roles
data RoleResponse = RoleResponse
  {
    rrkey  :: UUID,
    rrrole :: R.Role
  }
  deriving (Show)

-- | The response for retrieveing the roles
data UpdateRoleRequest = UpdateRoleRequest
  {
    urrkey  :: Maybe UUID,
    urrrole :: R.Role
  }
  deriving (Show)

instance ToJSON RoleResponse where
  -- this generates a Value
  toJSON (RoleResponse _key _role) =
    object ["role" .= object ["key" .= _key, "role" .= _role]]

instance ToJSON UserResponse where
  -- this generates a Value
  toJSON (UserResponse _key _username _email _active _token _password) =
    object ["user" .= object ["key" .= _key, "username" .= _username, "email" .= _email,
      "token" .= _token, "active" .= _active, "password" .= _password]]

instance FromJSON UpdateRoleRequest where
  parseJSON (Object v) = do
    w <- v .: "role"
    UpdateRoleRequest
      <$> w
      .:? "key"
      <*> w
      .: "role"
  parseJSON _ = empty

instance FromJSON UserResponse where
  parseJSON (Object v) = do
    w <- v .: "user"
    UserResponse
      <$> w
      .: "key"
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
-- | GET /api/users/count : Retrieves the number of registered users
-- | GET /api/users?offset=<int>&n=<int>       : Retrieves the list of <n> users beginning at <offset>
-- | GET /api/user/roles/<uuid> : Get all the roles for the specified user
-- | POST /api/user/roles/<uuid> : Update all roles and delete roles for the specified user
-- |
app :: (MonadIO m, MonadCatch m) => JScottyM m ()
app = do

  -- |Handles a login request and returns with the token if the user is valid and active
  post "/api/login" $ ( do
    req <- jsonData
    settings <- lift ask
    dbuser <- runDB $ DB.getBy $ UniqueUserUsername (lrusername req)
    case dbuser of
      Just (DB.Entity (UserKey key) user) | userActive user && authValidatePassword (userPassword user) (lrpassword req) -> do
        seconds <- liftIO $ fromIntegral . systemSeconds <$> getSystemTime
        let jwt = createToken
              ((C.key . C.token . config) settings)
              seconds
              ((C.valid . C.token . config) settings)
              ((C.issuer . C.token . config) settings)
              key
        let userResponse = UserResponse {key = key, username = (userUsername user), email = (userEmail user),
          active = (userActive user), token = Just jwt, password = Nothing}
        json userResponse
      _ -> status unauthorized401) `catch`
    (\(SomeException e) -> do
      status internalServerError500
      liftIO $ print e)

  -- |Checks a token and returns with the user if the user is valid and active
  get "/api/login" $ ( do
    kau <- getAuthenticated
    t <- getToken
    case kau of
      Just (UserKey k, u) -> do
        let userResponse = UserResponse { key = k, username = (userUsername u),
          email = (userEmail u), active = (userActive u), token = t, password = Nothing}
        json userResponse
      Nothing -> status unauthorized401) `catch`
    (\(SomeException e) -> do
      status internalServerError500
      liftIO $ print e)

  -- |Returns with the number of users
  get "/api/users/count" $ ( do
    roleRequired [R.Administrator]
    nof <- map DB.unSingle <$> runDB nofUsers
    json $ fromMaybe 0 $ listToMaybe nof) `catch`
    (\(SomeException e) -> do
      status internalServerError500
      liftIO $ print e)

  -- |Returns with the users roles based on the user key
  get "/api/user/:uuid/roles" $ ( do
    roleRequired [R.Administrator]
    uuid <- fromString <$> param "uuid"
    case uuid of
      Just k -> do
        dbroles <- runDB $ DB.selectList [AssignedRoleUser DB.==. UserKey k] []
        json $ map makeRoleResponse dbroles
      Nothing -> status notFound404) `catch`
    (\(SomeException e) -> do
      status internalServerError500
      liftIO $ print e)

  -- |Updates or add users roles based on the user key and role keys
  post "/api/user/:uuid/roles" $ ( do
    roleRequired [R.Administrator]
    uuid <- fromString <$> param "uuid"
    req <- jsonData
    case uuid of
      Just k -> do
        dbroles <- runDB $ DB.selectList [AssignedRoleUser DB.==. UserKey k] []
        let l = S.fromList $ map (unAssignedRoleKey . DB.entityKey) dbroles
            r = S.fromList $ map (fromJust . urrkey) (filter (isJust . urrkey) req)
            s = S.toList $ S.difference l r
        sequence_ $ map (runDB . prepareRoleUpdate k) req
        sequence_ $ map (runDB . prepareRoleDelete k) s
        status ok200
      Nothing -> status notFound404) `catch`
    (\(SomeException e) -> do
      status internalServerError500
      liftIO $ print e)

  -- |Returns with the use based on the key
  get "/api/user/:uuid" $ ( do
    roleRequired [R.Administrator]
    uuid <- fromString <$> param "uuid"
    case uuid of
      Just k -> do
        dbuser <- runDB $ DB.get $ UserKey k
        case dbuser of
          Just u -> do
            let userResponse = UserResponse { key = k, username = (userUsername u),
              email = (userEmail u), active = (userActive u), token = Nothing, password = Nothing}
            json userResponse
          Nothing -> status notFound404
      Nothing -> status badRequest400) `catch`
    (\(SomeException e) -> do
      status internalServerError500
      liftIO $ print e)

  -- |Creates a user
  put "/api/user" $ (do
    roleRequired [R.Administrator]
    req <- jsonData
    settings <- lift ask
    dbuser <- runDB $ DB.getBy $ UniqueUserUsername (curusername req)
    if isNothing dbuser then do
      pwd <- liftIO $ authHashPassword ((C.cost . C.password . config) settings) (curpassword req)
      (UserKey k) <- runDB $ DB.insert $ User { userUsername = (curusername req), userPassword = pwd, userEmail = (curemail req), userActive = (curactive req)}
      dbuser <- runDB $ DB.get $ UserKey k
      case dbuser of
          Just u -> do
            let userResponse = UserResponse { key = k, username = (userUsername u),
              email = (userEmail u), active = (userActive u), token = Nothing, password = Nothing}
            json userResponse
          Nothing -> do
            json $ JanusError { code = USR002, extra = Just $ toString k }
            status notFound404      
      status created201
    else do
      json $ JanusError { code = USR001, extra = Nothing }
      status conflict409
    ) `catch`
      (\(SomeException e) -> do
        json $ JanusError { code = JAN001, extra = Just $ show e }
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
        runDB $ DB.update (UserKey k) $ [ UserUsername DB.=. uurusername req,
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
  get "/api/users" $ ( do
    roleRequired [R.Administrator]
    settings <- lift ask
    start <- param "offset" `rescue` (\_ -> pure 0)
    nof <- param "n" `rescue` (\_ -> pure $ fromIntegral $ (C.length . C.ui . config) settings)
    dbl <- runDB $ DB.selectList [] [DB.LimitTo nof, DB.OffsetBy start, DB.Asc UserUsername]
    json $ map makeUserResponse dbl) `catch`
    (\(SomeException e) -> do
      status internalServerError500
      liftIO $ print e)

  where

    makeUserResponse::DB.Entity User -> UserResponse
    makeUserResponse (DB.Entity (UserKey key) u) = UserResponse {key=key, username = (userUsername u),
      email = (userEmail u), active = (userActive u), token = Nothing, password = Nothing}

    makeRoleResponse::DB.Entity AssignedRole -> RoleResponse
    makeRoleResponse (DB.Entity (AssignedRoleKey key) r) = RoleResponse { rrkey=key, rrrole = assignedRoleType r}

    prepareRoleUpdate::MonadIO m => UUID->UpdateRoleRequest->ReaderT DB.SqlBackend m (Key AssignedRole)
    prepareRoleUpdate k (UpdateRoleRequest Nothing r) = do
      DB.insert $ AssignedRole {assignedRoleType = r, assignedRoleUser = UserKey k}
    prepareRoleUpdate k (UpdateRoleRequest (Just key) r) = do
      DB.update (AssignedRoleKey key) [AssignedRoleType DB.=. r, AssignedRoleUser DB.=. UserKey k]
      pure $ AssignedRoleKey k

    prepareRoleDelete::MonadIO m => UUID->UUID->ReaderT DB.SqlBackend m ()
    prepareRoleDelete u k = DB.deleteWhere [AssignedRoleId DB.==. AssignedRoleKey k, AssignedRoleUser DB.==. UserKey u]
