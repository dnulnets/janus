{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Janus.Static
-- Description : The static part of the Janus application.
-- Copyright   : (c) Tomas Stenlund, 2023
-- License     : GNU
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains the static part of the application that servers static pages.
module Janus.User (app) where

import Control.Monad.Reader (ask)
import Control.Monad.Trans (lift, liftIO)
import Control.Applicative ( Alternative(empty) )
import Data.Text (Text)
import Data.Aeson
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    ToJSON (toJSON),
    Value (Object),
    object,
    (.:)
  )
import Janus.Core ( JScottyM ) 
import Data.Time.Clock.System
    ( SystemTime(systemSeconds), getSystemTime )
import Janus.Utils.JWT ( createToken )
import Janus.Data.Config (Config(..))
import Web.Scotty.Trans (json, post, jsonData)

-- |User information for the login response
data LoginResponse = LoginResponse {
  uid::Text,
  username::Text,
  email::Text,
  token::Text
} deriving (Show)

-- |User information for the login response
data LoginRequest = LoginRequest {
  qusername::Text,
  qpassword::Text
} deriving (Show)

instance ToJSON LoginResponse where

  -- this generates a Value
  toJSON (LoginResponse uid username email token) =
    object [ "user" .= object ["uid" .= uid, "username" .= username, "email" .= email, "token" .= token]]

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
app :: JScottyM ()
app = do
  -- The icon
  post "/api/users/login" $ do
    req <- jsonData
    liftIO $ showit req
    conf <- lift ask
    seconds <- liftIO $ fromIntegral . systemSeconds <$> getSystemTime
    let jwt = createToken (key conf) seconds (valid conf) (issuer conf) "64576432879"
    let user = LoginResponse {uid = "123121", username = "tomas", email = "tomas@stenlund.eu", token = jwt}
    json user
  where
    showit::LoginRequest->IO ()
    showit req = putStrLn $ show req