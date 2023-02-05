{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Janus.Data.User
-- Description : The user type that are used by the application
-- Copyright   : (c) Tomas Stenlund, 2023
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@telia.se
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains the user information regardless of interface.
module Janus.Data.User (User (..)) where

import Control.Applicative ( Alternative(empty) )
import Data.Text (Text)
import Data.Aeson
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    ToJSON (toEncoding, toJSON),
    Value (Object),
    object,
    pairs,
    (.:),
  )

data User = User
  { -- | User key
    uid :: Text,
    -- | The username used when looging in
    username :: Text,
    -- | The user email address
    email :: Text
  }
  deriving (Show)

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

instance ToJSON User where

  -- this generates a Value
  toJSON (User uid username email) =
    object ["uid" .= uid, "username" .= username, "email" .= email]

  -- this encodes directly to a bytestring Builder
  toEncoding (User uid username email) =
    pairs ("uid" .= uid <> "username" .= username <> "email" .= email)
