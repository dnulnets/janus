-- |
-- Module      : Janus.Data.Config
-- Description : The configuration of the application
-- Copyright   : (c) Tomas Stenlund, 2023
-- License     : GNU AFFERO GENERAL PUBLIC LICENSE
-- Maintainer  : tomas.stenlund@telia.se
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains the data type and handling for the application configuration
module Janus.Settings (Settings(..)) where

import Database.Persist.Postgresql (ConnectionPool)
import Janus.Data.Config

data Settings = Settings {
  config :: Config
  , dbpool :: ConnectionPool
}
