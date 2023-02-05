{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Janus.Data.Config (Config (..), readConfig) where

import Data.Text ( Text)
import Data.Yaml ( FromJSON (parseJSON), decodeFileEither, Value (Object), (.:) )
import Control.Applicative (Alternative(empty))

data Config = Config {
  issuer :: Text, 
  key :: Text, 
  valid :: Integer
} deriving (Show)

instance FromJSON Config where
  parseJSON (Object v) = do
    w <- v .: "token"
    Config
      <$> w
      .: "issuer"
      <*> w
      .: "key"
      <*> w
      .: "valid"
  parseJSON _ = empty

-- |Reads the configuration file for the Janus application
readConfig :: String->IO Config
readConfig f =
  either (error . show) id
    <$> decodeFileEither f
