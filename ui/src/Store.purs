module Janus.Store
  ( Action(..)
  , LogLevel(..)
  , Store
  , reduce
  )
  where

import Prelude

import Janus.Api.Request (BaseURL)
import Janus.Data.Profile (Profile)
import Data.Maybe (Maybe(..))

-- | Different types of log level
data LogLevel = Dev | Prod
derive instance eqLogLevel ∷ Eq LogLevel
derive instance ordLogLevel ∷ Ord LogLevel

-- | The store definition
type Store =
  { logLevel ∷ LogLevel
  , baseUrl ∷ BaseURL
  , currentUser ∷ Maybe Profile
  }

-- | The actions supported by the store.
data Action
  = LoginUser Profile -- ^ Logs in a user
  | LogoutUser        -- ^ Logs out a user

-- | Reduce the store based on an action.
reduce ∷ Store → Action → Store
reduce store = case _ of
  LoginUser profile →
    store { currentUser = Just profile }
  LogoutUser →
    store { currentUser = Nothing }
