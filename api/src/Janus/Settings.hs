{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Janus.Data.Config
-- Description : The configuration of the application
-- Copyright   : (c) Tomas Stenlund, 2023
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@telia.se
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains the data type and handling for the application configuration
module Janus.Settings where

import Data.Text (Text)
import Database.Persist.Postgresql
import Janus.Data.Config

data Settings = Settings {
  config :: Config
  , dbpool :: ConnectionPool
}
