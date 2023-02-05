-- |
-- Module      : Core
-- Description : Types and other definitions for the Janus application
-- Copyright   : (c) Tomas Stenlund, 2023
-- License     : GNU
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
--
module Janus.Core (JScottyM, JActionM) where

import Control.Monad.Trans.Reader ( ReaderT )
import Data.Text.Lazy ( Text )

import Web.Scotty.Trans ( ActionT, ScottyT )

import Janus.Data.Config ( Config )

-- |Type for the Janus Scotty Monad, has a reader for the config
type JScottyM = ScottyT Text (ReaderT Config IO)

-- |Type for the Janus Action Monad, has a reader for the config
type JActionM = ActionT Text (ReaderT Config IO)
