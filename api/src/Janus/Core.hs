{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Core
-- Description : Types and other definitions for the Janus application
-- Copyright   : (c) Tomas Stenlund, 2023
-- License     : GNU AFFERO GENERAL PUBLIC LICENSE
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Contains various generic types for the Janus application.
module Janus.Core (JScottyM, JActionM, getAuthenticated, getToken, authenticationRequired, roleRequired) where

import           Control.Monad                   (join)
import           Control.Monad.IO.Class          (MonadIO)
import           Control.Monad.Reader            (ask)
import           Control.Monad.Trans             (lift, liftIO)
import           Control.Monad.Trans.Reader      (ReaderT)
import qualified Data.Text                       as T
import           Data.Text.Encoding              (decodeUtf8', encodeUtf8)
import           Data.Text.Lazy                  (Text, toStrict)
import           Data.Time.Clock.System          (SystemTime (systemSeconds),
                                                  getSystemTime)
import qualified Database.Persist.Sql            as DB
import qualified Janus.Data.Config               as C
import           Janus.Data.Model                (Unique (UniqueUserGUID),
                                                  User (userActive))
import           Janus.Settings                  (Settings (..))
import           Janus.Utils.DB                  (runDB)
import           Janus.Utils.JWT                 (getSubject)
import           Network.Wai.Middleware.HttpAuth (extractBearerAuth)
import           Web.Scotty.Trans                (ActionT, ScottyT, header, finish, status)
import Network.HTTP.Types (unauthorized401)

-- | Type for the Janus Scotty Monad, has a reader for the config
type JScottyM m = ScottyT Text (ReaderT Settings m)

-- | Type for the Janus Action Monad, has a reader for the config
type JActionM m = ActionT Text (ReaderT Settings m)

-- |Returns with the user that is authenticated with this request.
getAuthenticated::(MonadIO m) => JActionM m (Maybe User)
getAuthenticated = do
    settings <- lift ask
    token <- getToken
    case token of
      Just b -> do
        seconds <- liftIO $ fromIntegral . systemSeconds <$> getSystemTime
        case getSubject (C.key (C.token (config settings))) seconds b (C.issuer (C.token (config settings))) of
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

-- |Shortcircuits the scotty action with a 401 if the user has no valid authentication.
roleRequired::(MonadIO m) => JActionM m ()
roleRequired = do
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