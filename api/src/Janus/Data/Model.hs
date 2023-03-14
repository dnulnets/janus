{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
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

import           Data.Text           (Text)
import Database.Persist.TH
    ( mkMigrate, mkPersist, persistLowerCase, share, sqlSettings )

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

User
    username Text
    guid Text
    password Text
    email Text
    UniqueUserUsername username
    UniqueUserGUID guid
    deriving Show

|]

