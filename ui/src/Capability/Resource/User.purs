module Janus.Capability.Resource.User
  ( UpdateProfileFields
  , CreateProfileFields
  , getCurrentUser
  , loginUser
  , updateUser
  , getUser
  , getUsers
  , createUser
  , deleteUser
  , nofUsers
  , getRoles
  , updateRoles
  , class ManageUser
  )
  where

import Prelude

import Data.Maybe (Maybe)
import Data.Either (Either)
import Halogen (HalogenM, lift)
import Janus.Api.Request (LoginFields)
import Janus.Data.Profile (Profile, ProfileBase)
import Janus.Data.UUID (UUID)
import Janus.Data.Role (Role)
import Janus.Data.Error

-- |The fields used by the api for updating the user profile, it contains the password if the user wants to change it.
type UpdateProfileFields = { | ProfileBase (key::UUID, password::Maybe String) }
type CreateProfileFields = { | ProfileBase (password::String) }

-- |The collection of user manipulation functions as well as the login functionality
class Monad m <= ManageUser m where
  loginUser :: LoginFields -> m (Maybe Profile)
  getCurrentUser :: m (Maybe Profile)
  createUser :: CreateProfileFields -> m (Either Error Profile)
  updateUser :: UpdateProfileFields -> m (Maybe Error)
  getUser :: UUID -> m (Either Error Profile)
  deleteUser :: UUID -> m (Maybe Error)
  getUsers :: Int->Int->m (Array Profile)
  nofUsers :: m (Int)
  getRoles :: UUID -> m (Array Role)
  updateRoles :: UUID -> Array Role -> m (Maybe Error)

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
  getRoles = lift <<< getRoles
  updateRoles i r = lift $ updateRoles i r 
