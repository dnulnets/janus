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

data LogLevel = Dev | Prod
derive instance eqLogLevel ∷ Eq LogLevel
derive instance ordLogLevel ∷ Ord LogLevel

type Store =
  { logLevel ∷ LogLevel
  , baseUrl ∷ BaseURL
  , currentUser ∷ Maybe Profile
  }

data Action
  = LoginUser Profile
  | LogoutUser

reduce ∷ Store → Action → Store
reduce store = case _ of
  LoginUser profile →
    store { currentUser = Just profile }
  LogoutUser →
    store { currentUser = Nothing }
