-- |This module contains the capability for logging of the application.
module Janus.Data.Log
  ( LogReason(..)
  , message
  , reason
  , Log
  , mkLog
  ) where

import Prelude

import Janus.Capability.Now (class Now, nowDateTime)
import Data.DateTime (DateTime)
import Data.Either (either)
import Data.Foldable (fold)
import Data.Formatter.DateTime (formatDateTime)

data LogReason = Debug | Info | Warn | Error
derive instance eqLogReason :: Eq LogReason
derive instance ordLogReason :: Ord LogReason

newtype Log = Log
  { reason :: LogReason
  , timestamp :: DateTime
  , message :: String
  }

derive instance eqLog :: Eq Log

message :: Log -> String
message (Log { message: m }) = m

reason :: Log -> LogReason
reason (Log { reason: r }) = r

mkLog :: forall m. Now m => LogReason -> String -> m Log
mkLog logReason inputMessage = do
  now <- nowDateTime

  let
    headerWith start =
      fold [ "[", start, ": ", formatTimestamp now, "]\n", inputMessage ]

    formattedLog = headerWith case logReason of
      Debug -> "DEBUG"
      Info -> "INFO"
      Warn -> "WARNING"
      Error -> "ERROR"

  pure $ Log { reason: logReason, timestamp: now, message: formattedLog }

  where
  formatTimestamp =
    either (const "(Failed to assign time)") identity
      <<< formatDateTime "YYYY-DD-MM hh:mm:ss a"
