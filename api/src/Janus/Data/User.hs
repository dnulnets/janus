{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Janus.Data.User
-- Description : The user type that are used by the application and various functions associated with the user.
-- Copyright   : (c) Tomas Stenlund, 2023
-- License     : GNU AFFERO GENERAL PUBLIC LICENSE
-- Maintainer  : tomas.stenlund@telia.se
-- Stability   : experimental
-- Portability : POSIX
--
-- The user type used within the server application.
module Janus.Data.User (User(..), nofUsers) where

import           Control.Applicative (Alternative (empty))
import           Data.Aeson          (FromJSON (parseJSON), KeyValue ((.=)),
                                      ToJSON (toEncoding, toJSON),
                                      Value (Object), object, pairs, (.:))
import           Data.Text           (Text)
import           Control.Monad.Trans         (MonadIO)
import           Database.Persist.Sql        (SqlBackend, Single, rawSql)
import           Control.Monad.Trans.Reader  (ReaderT)
                                              
-- | The user type, used by the application.
data User = User
  { -- | User key
    guid      :: Text,
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
      .: "guid"
      <*> v
      .: "username"
      <*> v
      .: "email"
  parseJSON _ = empty

-- | Json emitter for the user type.
instance ToJSON User where
  -- this generates a Value
  toJSON (User guid username email) =
    object ["guid" .= guid, "username" .= username, "email" .= email]

  -- this encodes directly to a bytestring Builder
  toEncoding (User guid username email) =
    pairs ("guid" .= guid <> "username" .= username <> "email" .= email)


nofUsers::MonadIO m => ReaderT SqlBackend m [Single Int]
nofUsers = rawSql "SELECT COUNT(*) FROM \"user\"" []