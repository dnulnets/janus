{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Janus.Data.Role
-- Description : The role type that are used by the application
-- Copyright   : (c) Tomas Stenlund, 2023
-- License     : GNU AFFERO GENERAL PUBLIC LICENSE
-- Maintainer  : tomas.stenlund@telia.se
-- Stability   : experimental
-- Portability : POSIX
--
-- The role type used within the server application.
module Janus.Data.Role (Role(..)) where

import           Control.Applicative (Alternative (empty))
import           Data.Aeson          (FromJSON (parseJSON), KeyValue ((.=)),
                                      ToJSON (toEncoding, toJSON),
                                      Value (Object), object, pairs, (.:),
                                      (.:?))
import           Data.Text           (Text)
import           Database.Persist.TH (derivePersistField)
import           GHC.Generics        (Generic)


-- | The roles supported by the application
data Role = User | Administrator | CreateObject | UpdateObject | DestroyObject | ReadObject
    deriving (Eq, Show, Read, Generic, Ord)
derivePersistField "Role"
instance FromJSON Role
instance ToJSON Role


-- | The user type, used by the application.
data AssignedRole = AssignedRole
  { -- | What kind of role it is
    role   :: Role
    -- | Object key, if any
    , guid :: Maybe Text }
  deriving (Show)

-- | Json parser for the user type.
instance FromJSON AssignedRole where
  parseJSON (Object v) =
    AssignedRole
      <$> v .: "role"
      <*> v .:? "guid"
  parseJSON _          = empty

-- | Json emitter for the user type.
instance ToJSON AssignedRole where
  -- this generates a Value
  toJSON ar =
    object ["role" .= role ar, "guid" .= guid ar]

  -- this encodes directly to a bytestring Builder
  toEncoding ar =
    pairs ("role" .= role ar <> "guid" .= guid ar)
