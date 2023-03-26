module Janus.Capability.Resource.User
  ( UpdateProfileFields
  , getCurrentUser
  , loginUser
  , updateUser
  , getUser
  , getUsers
  , class ManageUser
  )
  where

import Prelude

import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Janus.Api.Request (LoginFields)
import Janus.Data.Profile (Profile, ProfileBase)
import Janus.Data.UUID (UUID)

-- |The fields used by the api for updating the user profile, it contains the password if the user wants to change it.
type UpdateProfileFields = { password::Maybe String | ProfileBase () }

-- |The collection of user manipulation functions as well as the login functionality
class Monad m <= ManageUser m where
  loginUser :: LoginFields -> m (Maybe Profile)
  getCurrentUser :: m (Maybe Profile)
  updateUser :: UpdateProfileFields -> m Unit
  getUser :: UUID -> m (Maybe Profile)
  getUsers :: m (Array Profile)

-- |Helper to avoid lifting
instance manageUserHalogenM :: ManageUser m => ManageUser (HalogenM st act slots msg m) where
  loginUser = lift <<< loginUser
  getCurrentUser = lift getCurrentUser
  updateUser = lift <<< updateUser
  getUser = lift <<< getUser
  getUsers = lift getUsers
