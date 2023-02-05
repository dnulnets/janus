module Janus.Capability.Navigate where

import Prelude

import Janus.Data.Route (Route)
import Control.Monad.Trans.Class (lift)
import Halogen (HalogenM)

-- |Specifies the navigate and logout functions
class Monad m <= Navigate m where
  navigate :: Route -> m Unit
  logout :: m Unit

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance navigateHalogenM :: Navigate m => Navigate (HalogenM st act slots msg m) where
  navigate = lift <<< navigate
  logout = lift logout
