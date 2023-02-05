module Janus.Capability.LogMessages where

import Prelude

import Janus.Capability.Now (class Now)
import Janus.Data.Log (Log, LogReason(..), mkLog)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen (HalogenM)

-- |The log function for the Janus application
class Monad m <= LogMessages m where
  logMessage :: Log -> m Unit

-- |Help us avoid lifting
instance logMessagesHalogenM :: LogMessages m => LogMessages (HalogenM st act slots msg m) where
  logMessage = lift <<< logMessage

-- |Logs a message
log :: forall m. LogMessages m => Now m => LogReason -> String -> m Unit
log reason = logMessage <=< mkLog reason

-- |Log a debug message
logDebug :: forall m. LogMessages m => Now m => String -> m Unit
logDebug = log Debug

-- |Log an informational message
logInfo :: forall m. LogMessages m => Now m => String -> m Unit
logInfo = log Info

-- |Log a warning message
logWarn :: forall m. LogMessages m => Now m => String -> m Unit
logWarn = log Warn

-- |Log a error message
logError :: forall m. LogMessages m => Now m => String -> m Unit
logError = log Error

-- |Run a monadic action and hush the message if any, but log it
logHush :: forall m a. LogMessages m => Now m => LogReason -> m (Either String a) -> m (Maybe a)
logHush reason action =
  action >>= case _ of
    Left e -> case reason of
      Debug -> logDebug e *> pure Nothing
      Info -> logInfo e *> pure Nothing
      Warn -> logWarn e *> pure Nothing
      Error -> logError e *> pure Nothing
    Right v -> pure $ Just v

-- |Run a monadic action and hush the message and force it into debug, but log it
debugHush :: forall m a. LogMessages m => Now m => m (Either String a) -> m (Maybe a)
debugHush = logHush Debug
