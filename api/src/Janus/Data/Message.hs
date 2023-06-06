{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Janus.Data.Message
-- Description : Messages returned from the API for info, errors and warnings.
-- Copyright   : (c) Tomas Stenlund, 2023
-- License     : GNU AFFERO GENERAL PUBLIC LICENSE
-- Maintainer  : tomas.stenlund@telia.se
-- Stability   : experimental
-- Portability : POSIX
--
-- The role type used within the server application.
module Janus.Data.Message where


import           Control.Applicative (Alternative (empty))
import           GHC.Generics        (Generic)
import           Data.Aeson          (FromJSON (parseJSON), KeyValue ((.=)),
                                      ToJSON (toEncoding, toJSON),
                                      Value (Object), object, pairs, (.:), (.:?))

-- | The error codes supported by the application
data Message = JAN001 -- Unknown error
    | JAN002 -- Communication problem with the server (client side error)
    | JAN003 -- System error, typical 500
    | JAN004 -- Not authorized
    | USR001      -- Username already exists
    deriving (Eq, Show, Read, Generic, Ord)
instance FromJSON Message
instance ToJSON Message

data JanusError = JanusError {
        code::Message
        , extra::Maybe String
    }

-- | Json parser for the user type.
instance FromJSON JanusError where
  parseJSON (Object v) =
    JanusError
      <$> v
      .: "code"
      <*> v
      .:? "extra"
  parseJSON _ = empty

-- | Json emitter for the user type.
instance ToJSON JanusError where
  -- this generates a Value
  toJSON (JanusError code extra ) =
    object $ ["code" .= code] <> maybe [] (const ["extra" .= extra]) extra

  -- this encodes directly to a bytestring Builder
  toEncoding (JanusError code extra) =
    pairs $ maybe ("code" .= code) (const ("code" .= code <> "extra" .= extra)) extra
