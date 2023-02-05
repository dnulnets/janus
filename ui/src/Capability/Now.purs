module Janus.Capability.Now where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.DateTime (Date, DateTime, Time)
import Data.DateTime.Instant (Instant)
import Halogen (HalogenM)

-- | Specifies the time functions to be used in our monad.
class Monad m <= Now m where
  now :: m Instant
  nowDate :: m Date
  nowTime :: m Time
  nowDateTime :: m DateTime

-- | Let us avoid lifting.
instance nowHalogenM :: Now m => Now (HalogenM st act slots msg m) where
  now = lift now
  nowDate = lift nowDate
  nowTime = lift nowTime
  nowDateTime = lift nowDateTime
