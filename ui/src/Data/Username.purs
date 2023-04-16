-- |This module contains the username data type and some additional functions for json and
-- |other helper functions.
module Janus.Data.Username
  ( Username
  , parse
  , codec
  ) where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap)

newtype Username = Username String

derive instance eqUsername :: Eq Username
derive instance ordUsername :: Ord Username

instance showUsername :: Show Username where
  show (Username n) = n 

-- |The json codec for the datatype.
codec :: JsonCodec Username
codec = dimap (\(Username user) -> user) Username CA.string

-- ª|Parses a string to a user name.
parse :: String -> Maybe Username
parse "" = Nothing
parse str = Just (Username str)

