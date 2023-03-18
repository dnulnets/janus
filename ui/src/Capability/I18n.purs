module Janus.Capability.I18n where

import Prelude
import Halogen (HalogenM, lift)
import Simple.I18n.Translator (Translator)

-- |The collection of user manipulation functions as well as the login functionality
class Monad m <= I18n m where
  country::m String

-- |Helper to avoid lifting
instance i18nHalogenM :: I18n m => I18n (HalogenM st act slots msg m) where
  country = lift country


