{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Janus.Data.UUID(UUID, uuidDef) where


import qualified Data.ByteString.Char8          as C8
import qualified Data.Text as T
import           Data.UUID
import           Database.Persist               (LiteralType (Escaped),
                                                 PersistField,
                                                 PersistValue (..),
                                                 SqlType (SqlOther))
import           Database.Persist.ImplicitIdDef (ImplicitIdDef, mkImplicitIdDef)
import           Database.Persist.Postgresql    (PersistField (..),
                                                 PersistFieldSql)
import           Database.Persist.Sql           (PersistFieldSql (sqlType))
import           Prelude
import Data.Either.Extra (maybeToEither)
import Web.PathPieces

uuidDef :: ImplicitIdDef
uuidDef = mkImplicitIdDef @UUID "gen_random_uuid()"
--uuidDef = mkImplicitIdDef @UUID "uuid_generate_v4()"

instance PersistField UUID where
    toPersistValue uuid =
        PersistLiteral_ Escaped $ C8.pack $ toString uuid

    fromPersistValue (PersistLiteral_ Escaped s) = maybeToEither "Unable to parse UUID" (fromString $ C8.unpack s)
    fromPersistValue _ = Left "Unknow literal for UUID"

instance PersistFieldSql UUID where
    sqlType _ = SqlOther "UUID"

instance PathPiece UUID where
  toPathPiece u = T.pack $ toString u
  fromPathPiece s = fromString $ T.unpack s