{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Janus.Data.User
-- Description : The user type that are used by the application
-- Copyright   : (c) Tomas Stenlund, 2023
-- License     : GNU AFFERO GENERAL PUBLIC LICENSE
-- Maintainer  : tomas.stenlund@telia.se
-- Stability   : experimental
-- Portability : POSIX
--
-- The user type used within the server application.
module Janus.Data.User (User (..)) where

import           Control.Applicative (Alternative (empty))
import           Data.Aeson          (FromJSON (parseJSON), KeyValue ((.=)),
                                      ToJSON (toEncoding, toJSON),
                                      Value (Object), object, pairs, (.:))
import           Data.Text           (Text)

-- | The user type, used by the application.
data User = User
  { -- | User key
    uid      :: Text,
    -- | The username used when looging in
    username :: Text,
    -- | The user email address
    email    :: Text
  }
  deriving (Show)

-- | Json parser for the user type.
instance FromJSON User where
  parseJSON (Object v) =
    User
      <$> v
      .: "uid"
      <*> v
      .: "username"
      <*> v
      .: "email"
  parseJSON _ = empty

-- | Json emitter for the user type.
instance ToJSON User where
  -- this generates a Value
  toJSON (User uid username email) =
    object ["uid" .= uid, "username" .= username, "email" .= email]

  -- this encodes directly to a bytestring Builder
  toEncoding (User uid username email) =
    pairs ("uid" .= uid <> "username" .= username <> "email" .= email)
