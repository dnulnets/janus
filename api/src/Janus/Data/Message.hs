{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Janus.Data.Message
-- Description : Messages returned from the API during info, errors and warnings.
-- Copyright   : (c) Tomas Stenlund, 2023
-- License     : GNU AFFERO GENERAL PUBLIC LICENSE
-- Maintainer  : tomas.stenlund@telia.se
-- Stability   : experimental
-- Portability : POSIX
--
-- The role type used within the server application.
module Janus.Data.Message where

import           Data.Aeson          (FromJSON,ToJSON)
import           GHC.Generics        (Generic)


-- | The roles supported by the application
data Message = USR001 | USR002
    deriving (Eq, Show, Read, Generic, Ord)
instance FromJSON Message
instance ToJSON Message
