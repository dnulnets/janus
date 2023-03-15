module Janus.Api.Endpoint
  ( Endpoint(..)
  , endpointCodec
  )
  where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', prefix, root)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

-- | The endpoints used by the Janus application, they are supported by the backend haskell
-- | server.
data Endpoint
  = Login 
  | User
  
derive instance genericEndpoint :: Generic Endpoint _

-- | The codec for the valid routes in the Janus application.
endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ prefix "api" $ sum
  { "Login": "user" / "login" / noArgs
  , "User": "user" / noArgs
  }
