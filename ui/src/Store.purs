-- | The module for the global storage. It defines the store, its reduce function and the
-- | actions the store supports.
module Janus.Store
  ( Action(..)
  , LogLevel(..)
  , Store
  , reduce
  ) where

import Prelude

import Janus.Api.Request (BaseURL)
import Janus.Data.Profile (Profile)
import Data.Maybe (Maybe(..))

-- | Different types of log level
data LogLevel = Dev | Prod

derive instance eqLogLevel ∷ Eq LogLevel
derive instance ordLogLevel ∷ Ord LogLevel

-- | The definition of the global store. It contains the application loglevel, the base URL and the current user.
type Store =
  { logLevel ∷ LogLevel
  , baseUrl ∷ BaseURL
  , currentUser ∷ Maybe Profile
  , locale :: String
  }

-- | The actions supported by the store.
data Action
  = LoginUser Profile -- | Logs in a user
  | LogoutUser -- | Logs out a user
  | Locale String -- | Locale code

-- | Reduces the store based on an action.
reduce ∷ Store → Action → Store
reduce store = case _ of
  LoginUser profile →
    store { currentUser = Just profile }
  LogoutUser →
    store { currentUser = Nothing }
  Locale c ->
    store { locale = c }

