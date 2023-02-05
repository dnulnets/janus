module Janus.Data.UUID
  ( UUID(..)
  , codec
  )
  where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Newtype (class Newtype)
import Data.Profunctor (dimap)

-- |Convenience type for handling UUID:s
newtype UUID = UUID String
derive instance ntUUID :: Newtype UUID _
derive instance eqUUID :: Eq UUID

-- |Codec for encoding and decoding a UUID
codec :: JsonCodec UUID
codec = dimap (\(UUID uuid) -> uuid) UUID CA.string
