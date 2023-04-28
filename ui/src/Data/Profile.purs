-- |This module contains the available profiles and codecs for json.
module Janus.Data.Profile
  ( NewProfile
  , Profile(..)
  , ProfileBase(..)
  , ProfileWithPassword
  , newProfileCodec
  , profileCodec
  , profileWithPasswordCodec
  )
  where

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe)
import Janus.Data.Email (Email)
import Janus.Data.Email as Email
import Janus.Data.UUID (UUID)
import Janus.Data.UUID as UUID
import Janus.Data.Username (Username)
import Janus.Data.Username as Username

type ProfileBase r = ( email :: Email, username::Username, active::Boolean | r )
type Profile = { | ProfileBase (key::UUID)}
type ProfileWithPassword = {password::Maybe String | ProfileBase(key::UUID)}
type NewProfile = { password::String | ProfileBase () }

profileCodec :: JsonCodec Profile
profileCodec =
  CAR.object "Profile"
    { key: UUID.codec,
      username: Username.codec,
      email: Email.codec,
      active: CA.boolean
    }

profileWithPasswordCodec :: JsonCodec ProfileWithPassword
profileWithPasswordCodec =
  CAR.object "ProfileWithPassword"
    { key: UUID.codec,
      username: Username.codec,
      email: Email.codec,
      active: CA.boolean,
      password: CAC.maybe CA.string
    }


newProfileCodec :: JsonCodec NewProfile
newProfileCodec =
  CAR.object "NewProfile"
    { username: Username.codec,
      email: Email.codec,
      active: CA.boolean,
      password: CA.string
    }
