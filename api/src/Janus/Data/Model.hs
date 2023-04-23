{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
-- |
-- Module      : Janus.Data.Model
-- Description : Application database model
-- Copyright   : (c) Tomas Stenlund, 2023
-- License     : GNU AFFERO GENERAL PUBLIC LICENSE
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
--
-- The database model for the application, it uses the persistence framework.
--
module Janus.Data.Model where

import           Data.Text                      (Text)
import           Database.Persist.TH            (mkMigrate,
                                                 mkPersist, persistLowerCase,
                                                 share, sqlSettings, setImplicitIdDef)
import qualified Janus.Data.Role                as Role
import           Janus.Data.UUID
import           Prelude

share [mkPersist (setImplicitIdDef uuidDef sqlSettings), mkMigrate "migrateAll"] [persistLowerCase|

User
    username Text
    password Text
    email Text
    active Bool
    UniqueUserUsername username
    deriving Show

AssignedRole
    type Role.Role
    user UserId
    object UUID Maybe
|]

