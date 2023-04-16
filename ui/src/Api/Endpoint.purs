module Janus.Api.Endpoint
  ( Endpoint(..)
  , endpointCodec
  )
  where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Janus.Data.UUID (UUID, uuid)
import Routing.Duplex (RouteDuplex', prefix, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

-- | The endpoints used by the Janus application, they are supported by the backend haskell
-- | server.
data Endpoint
  = Login 
  | CreateUser
  | Users
  | User UUID
  | NofUsers

derive instance genericEndpoint :: Generic Endpoint _

-- | The codec for the valid routes in the Janus application.
endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ prefix "api" $ sum
  { "Login": "login" / noArgs
  , "CreateUser": "user" / noArgs
  , "Users": "users" / noArgs
  , "User": "user" / uuid segment
  , "NofUsers": "users" / "count" / noArgs
  }
