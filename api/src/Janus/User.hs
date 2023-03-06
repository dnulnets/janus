{-# LANGUAGE OverloadedStrings #-}

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
module Janus.User (app) where

import Control.Applicative (Alternative (empty))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ask)
import Control.Monad.Trans (lift, liftIO)
import Data.Aeson
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    ToJSON (toJSON),
    Value (Object),
    object,
    (.:),
  )
import Data.Text (Text)
import Data.Time.Clock.System
  ( SystemTime (systemSeconds),
    getSystemTime,
  )
import Database.Persist.Sql
import Janus.Core (JScottyM)
import qualified Janus.Data.Config as C
import Janus.Data.Model
import Janus.Settings
import Janus.Utils.DB
import Janus.Utils.JWT (createToken)
import Janus.Utils.Password
import Network.HTTP.Types.Status
import Web.Scotty.Trans (json, jsonData, post, status)
-- import Web.Scotty (status)

-- | User information for the login response
data LoginResponse = LoginResponse
  { uid :: Text,
    username :: Text,
    email :: Text,
    token :: Text
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
  toJSON (LoginResponse _uid _username _email _token) =
    object ["user" .= object ["uid" .= _uid, "username" .= _username, "email" .= _email, "token" .= _token]]

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
    dbuser <- liftIO $ runDB (dbpool settings) $ getBy $ UniqueUid $ qusername req
    case dbuser of
      Just (Entity _ user) | authValidatePassword (userPassword user) (qpassword req) -> do
        seconds <- liftIO $ fromIntegral . systemSeconds <$> getSystemTime
        let jwt = createToken (C.key (C.token (config settings))) seconds (C.valid (C.token (config settings))) (C.issuer (C.token (config settings))) (userGuid user)
        let userResponse = LoginResponse {uid = (userGuid user), username = (userUid user), email = (userEmail user), token = jwt}
        json userResponse
      _ -> status unauthorized401
