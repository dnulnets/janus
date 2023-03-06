-- |
-- Module      : Core
-- Description : Types and other definitions for the Janus application
-- Copyright   : (c) Tomas Stenlund, 2023
-- License     : GNU AFFERO GENERAL PUBLIC LICENSE
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Contains various generic types for the Janus application.
module Janus.Core (JScottyM, JActionM) where

import Control.Monad.Trans.Reader (ReaderT)
import Data.Text.Lazy (Text)
import Janus.Settings (Settings)
import Web.Scotty.Trans (ActionT, ScottyT)

-- | Type for the Janus Scotty Monad, has a reader for the config
type JScottyM m = ScottyT Text (ReaderT Settings m)

-- | Type for the Janus Action Monad, has a reader for the config
type JActionM m = ActionT Text (ReaderT Settings m)
