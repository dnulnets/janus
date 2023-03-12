{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Janus
-- Description : Application configuration
-- Copyright   : (c) Tomas Stenlund, 2023
-- License     : GNU AFFERO GENERAL PUBLIC LICENSE
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains the data type and handling and parsing of the application configuration
-- located in a yaml-file.
--
module Janus.Data.Config (Database (..), Token (..), Config (..), readConfig) where

import           Control.Applicative (Alternative (empty))
import           Data.Text           (Text)
import           Data.Yaml           (FromJSON (parseJSON), Value (Object),
                                      decodeFileEither, (.:))

-- | Database configuration
data Database = Database
  { -- | The url to the database
    url  :: Text,
    -- | The size of the connection pool
    size :: Int
  }
  deriving (Show)

-- | Token generation and validation configuration
data Token = Token
  { -- | The issuer of the token
    issuer :: Text,
    -- | The secret key used to sign and encrypt the token
    key    :: Text,
    -- | The validity time of the token in seconds
    valid  :: Integer
  }
  deriving (Show)

-- | Application configuration
data Config = Config
  { -- | The token configuration
    token    :: Token,
    -- | The database configuration
    database :: Database
  }
  deriving (Show)

-- | The instance for json parsing of the configuration
instance FromJSON Config where
  parseJSON (Object v) = do
    w <- v .: "token"
    x <- v .: "database"
    Config
      <$> ( Token
              <$> w
              .: "issuer"
              <*> w
              .: "key"
              <*> w
              .: "valid"
          )
      <*> ( Database
              <$> x
              .: "url"
              <*> x
              .: "size"
          )
  parseJSON _ = empty

-- | Reads the configuration file for the Janus application
readConfig ::
  -- | The location and name of the configuration file
  String ->
  -- | The read configuration
  IO Config
readConfig f =
  either (error . show) id
    <$> decodeFileEither f
