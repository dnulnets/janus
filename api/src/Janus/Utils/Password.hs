-- |
-- Module      : Janus.Utils.Passwords
-- Description : Password handling functions.
-- Copyright   : (c) Tomas Stenlund, 2023
-- License     : GNU AFFERO GENERAL PUBLIC LICENSE
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains functionality to generate and verify bcrypted passwords.
module Janus.Utils.Password
  ( authHashPassword,
    authValidatePassword,
  )
where

--
-- External imports
--
import Crypto.KDF.BCrypt (hashPassword, validatePassword)

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

-- | Validates a password by checking a hashed password with the supplied password
authValidatePassword ::
  -- | The hashed password
  Text ->
  -- | The user supplied password in clear text
  Text ->
  -- | True if they match, otherwise false
  Bool
authValidatePassword hpwd upwd =
  validatePassword (encodeUtf8 upwd) (encodeUtf8 hpwd)

-- | Hashes a password.
authHashPassword ::
  -- | The cost of the hashing work
  Integer ->
  -- | The user supplied password in clear text
  Text ->
  -- | The hashed password
  IO Text
authHashPassword cost pwd = decodeUtf8 <$> hashPassword (fromIntegral cost) (encodeUtf8 pwd)
