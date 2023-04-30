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
data Role = User | Administrator | TeamLeader
    deriving (Eq, Show, Read, Generic, Ord)
derivePersistField "Role"
instance FromJSON Role
instance ToJSON Role
