module Janus.Capability.Resource.User
  ( UpdateProfileFields
  , getCurrentUser
  , loginUser
  , updateUser
  , getUser
  , getUsers
  , createUser
  , deleteUser
  , nofUsers
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
  createUser :: UpdateProfileFields -> m Unit
  updateUser :: UpdateProfileFields -> m Unit
  getUser :: UUID -> m (Maybe Profile)
  deleteUser :: UUID -> m Unit
  getUsers :: Int->Int->m (Array Profile)
  nofUsers :: m (Int)

-- |Helper to avoid lifting
instance manageUserHalogenM :: ManageUser m => ManageUser (HalogenM st act slots msg m) where
  loginUser = lift <<< loginUser
  getCurrentUser = lift getCurrentUser
  createUser = lift <<< createUser
  updateUser = lift <<< updateUser
  getUser = lift <<< getUser
  deleteUser = lift <<< deleteUser
  getUsers o n = lift $ getUsers o n
  nofUsers = lift nofUsers


