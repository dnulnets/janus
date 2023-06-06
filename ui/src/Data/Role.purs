module Janus.Data.Role
  ( Role(..)
  , RoleType(..)
  , allRoles
  , codec
  , roleCodec
  ) where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Sum (enumSum)
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe(..))
import Janus.Data.UUID (UUID)
import Janus.Data.UUID as UUID

data RoleType = User | Administrator | TeamLeader
derive instance eqRoleType :: Eq RoleType
derive instance ordRoleType :: Ord RoleType

instance showRole :: Show RoleType where
  show r = case r of
    User -> "User"
    Administrator -> "Administrator"
    TeamLeader -> "TeamLeader"

read::String->Maybe RoleType
read = case _ of
    "User" -> Just User
    "Administrator" -> Just Administrator
    "TeamLeader" -> Just TeamLeader
    _ -> Nothing

codec :: JsonCodec RoleType
codec = enumSum show read

type Role = { key::Maybe UUID, role::RoleType }

roleCodec :: JsonCodec Role
roleCodec =
  CAR.object "Role"
    { key: CAC.maybe UUID.codec,
      role: codec
    }

allRoles::Array Role
allRoles = [{key:Nothing, role:User}, {key:Nothing, role:Administrator}, {key:Nothing, role:TeamLeader}]