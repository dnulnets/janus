module Janus.Data.Profile
  ( Profile(..)
  , ProfileBase(..)
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

type ProfileBase r = ( uid::UUID, email :: Email, username::Username | r )
type Profile = { | ProfileBase ()}
type ProfileWithPassword = {| ProfileBase(password::Maybe String)}

profileCodec :: JsonCodec Profile
profileCodec =
  CAR.object "Profile"
    { uid: UUID.codec,
      username: Username.codec,
      email: Email.codec
    }

profileWithPasswordCodec :: JsonCodec ProfileWithPassword
profileWithPasswordCodec =
  CAR.object "ProfileWithPassword"
    { uid: UUID.codec,
      username: Username.codec,
      email: Email.codec,
      password: CAC.maybe CA.string
    }
