-- |This module contains support for a UUID type and encoding, decoding of the type for json. 
module Janus.Data.UUID where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Profunctor (dimap)
import Routing.Duplex (RouteDuplex', as)

-- |Convenience type for handling UUID:s
newtype UUID = UUID String
derive instance ntUUID :: Newtype UUID _
derive instance eqUUID :: Eq UUID
derive instance ordUUID :: Ord UUID
derive instance genericUUID :: Generic UUID _

instance showUUID :: Show UUID where
  show (UUID n) = n 

-- |Codec for encoding and decoding a UUID
codec :: JsonCodec UUID
codec = dimap (\(UUID u) -> u) UUID CA.string

-- |A route duplex for UUID. To be used for the endpoints
uuid :: RouteDuplex' String -> RouteDuplex' UUID
uuid = as uuidToString uuidFromString

  where

    uuidFromString :: String -> Either String UUID
    uuidFromString s = Right $ UUID s

    uuidToString :: UUID -> String
    uuidToString (UUID s) = s
