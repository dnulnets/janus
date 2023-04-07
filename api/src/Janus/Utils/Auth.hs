{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Auth
-- Description : Functions for handling authentication and authroization checks.
-- Copyright   : (c) Tomas Stenlund, 2023
-- License     : GNU AFFERO GENERAL PUBLIC LICENSE
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Contains various functions for checking the authentication and authorization of the
-- user making the request.
module Janus.Utils.Auth (getAuthenticated, getToken, authenticationRequired, roleRequired) where

import           Control.Monad                   (join)
import           Control.Monad.IO.Class          (MonadIO)
import           Control.Monad.Reader            (ask)
import           Control.Monad.Trans             (lift, liftIO)
import qualified Data.Set                        as DS
import qualified Data.Text                       as T
import           Data.Text.Encoding              (decodeUtf8', encodeUtf8)
import           Data.Text.Lazy                  (toStrict)
import           Data.Time.Clock.System          (SystemTime (systemSeconds),
                                                  getSystemTime)
import           Database.Persist.Sql            ((==.))
import qualified Database.Persist.Sql            as DB
import           Janus.Core                      (JActionM)
import qualified Janus.Data.Config               as C
import           Janus.Data.Model                (AssignedRole (assignedRoleType),
                                                  EntityField (AssignedRoleUser),
                                                  Unique (UniqueUserGUID),
                                                  User (userActive))
import           Janus.Data.Role
import           Janus.Settings                  (Settings (..))
import           Janus.Utils.DB                  (runDB)
import           Janus.Utils.JWT                 (getSubject)
import           Network.HTTP.Types              (unauthorized401)
import           Network.Wai.Middleware.HttpAuth (extractBearerAuth)
import           Web.Scotty.Trans                (finish, header, status)

-- |Returns with the user that is authenticated with this request.
getAuthenticated::(MonadIO m) => JActionM m (Maybe User)
getAuthenticated = do
    settings <- lift ask
    token <- getToken
    case token of
      Just b -> do
        seconds <- liftIO $ fromIntegral . systemSeconds <$> getSystemTime
        case getSubject ((C.key . C.token . config) settings) seconds b ((C.issuer . C.token . config) settings) of
          Just u -> do
            dbuser <- runDB $ DB.getBy $ UniqueUserGUID u
            case dbuser of
              Just (DB.Entity _ user) | userActive user -> pure $ Just user
              _                                         -> pure Nothing
          Nothing -> pure Nothing
      _ -> pure Nothing

-- |Returns with the token that came with the request.
getToken::(MonadIO m) => JActionM m (Maybe T.Text)
getToken = do
    auth <- header "Authorization"
    let bearer = extractBearerAuth . encodeUtf8 . toStrict <$> auth
    case decodeUtf8' <$> join bearer of
      Just (Right b) -> pure $ Just b
      _              -> pure Nothing

-- |Shortcircuits the scotty action with a 401 if the user has no valid authentication.
authenticationRequired::(MonadIO m) => JActionM m ()
authenticationRequired = do
    settings <- lift ask
    token <- getToken
    case token of
      Just b -> do
        seconds <- liftIO $ fromIntegral . systemSeconds <$> getSystemTime
        case getSubject (C.key (C.token (config settings))) seconds b (C.issuer (C.token (config settings))) of
          Just u -> do
            dbuser <- runDB $ DB.getBy $ UniqueUserGUID u
            case dbuser of
              Just (DB.Entity _ user) | userActive user -> pure ()
              _                                         -> do
                status unauthorized401
                finish
          Nothing -> do
            status unauthorized401
            finish
      _ -> do
        status unauthorized401
        finish

-- |Shortcircuits the scotty action with a 401 if the user has no valid authentication and at least one of the provided roles.
roleRequired::(MonadIO m) => [Role] -> JActionM m ()
roleRequired lor = do
    settings <- lift ask
    token <- getToken
    case token of
      Just b -> do
        seconds <- liftIO $ fromIntegral . systemSeconds <$> getSystemTime
        case getSubject (C.key (C.token (config settings))) seconds b (C.issuer (C.token (config settings))) of
          Just u -> do
            dbuser <- runDB $ DB.getBy $ UniqueUserGUID u
            case dbuser of
              Just (DB.Entity key user) | userActive user -> do
                roles <- runDB $ DB.selectList [AssignedRoleUser ==. key] []
                let h = intersectIsNotEmpty lor $ map extractRoles roles
                if h
                  then pure()
                else do
                  status unauthorized401
                  finish
              _                                         -> do
                status unauthorized401
                finish
          Nothing -> do
            status unauthorized401
            finish
      _ -> do
        status unauthorized401
        finish

    where

      extractRoles::DB.Entity AssignedRole -> Role
      extractRoles (DB.Entity _ r) = assignedRoleType r

      intersectIsNotEmpty::[Role]->[Role]->Bool
      intersectIsNotEmpty l1 l2 = not $ DS.null $ DS.intersection (DS.fromList l1) (DS.fromList l2)
