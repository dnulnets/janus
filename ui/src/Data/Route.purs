module Janus.Data.Route where

import Prelude hiding ((/))

import Janus.Data.Username (Username)
import Janus.Data.Username as Username
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', as, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Slug (Slug)
import Slug as Slug

data Route
  = Home |
    Login |
    Dashboard

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "Home": noArgs
  , "Login": "login" / noArgs
  , "Dashboard": "dashboard" / noArgs
  , "Settings": "settings" / noArgs
  , "ViewArticle": "article" / slug segment
  , "Profile": "profile" / uname segment
  , "Favorites": "profile" / uname segment / "favorites"
  }

slug :: RouteDuplex' String -> RouteDuplex' Slug
slug = as Slug.toString (Slug.parse >>> note "Bad slug")

uname :: RouteDuplex' String -> RouteDuplex' Username
uname = as Username.toString (Username.parse >>> note "Bad username")
