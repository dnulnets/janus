{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Heat.Utils.JWT
-- Description : JSON Web Token functions
-- Copyright   : (c) Tomas Stenlund, 2023
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains functions to create and verify JSON Web Token
-- and to extract and insert the unregistered claim as a JSON structure
-- where you can store relevant information for your application
module Janus.Utils.JWT
  ( createToken,
    getSubject,
  )
where

import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import Web.JWT as JWT
    ( claims,
      decodeAndVerifySignature,
      encodeSigned,
      hmacSecret,
      numericDate,
      stringOrURI,
      stringOrURIToText,
      toVerify,
      Algorithm(HS256),
      JOSEHeader(alg, typ),
      JWTClaimsSet(sub, iat, exp, iss),
      NumericDate )

-- | Create a token
createToken ::
  -- | The secret used for signing
  Text ->
  -- | The time when the token was created from the epoch
  NominalDiffTime ->
  -- | Number of sceonds the token is validity from creation time
  Integer ->
  -- | Issuer
  Text ->
  -- | The subject
  Text ->
  -- | The token
  Text
createToken jwtSecret ndt len issuer subject =
  encodeSigned
    (JWT.hmacSecret jwtSecret)
    mempty {typ = Just "JWT", alg = Just HS256}
    mempty
      { JWT.iat = numericDate ndt,
        JWT.exp = numericDate (ndt + fromIntegral len),
        JWT.sub = stringOrURI subject,
        JWT.iss = stringOrURI issuer
      }

-- | Verifies and extracts a subject from the token.
getSubject ::
  -- | The secret to verify the signature with
  Text ->
  -- | The time compared with the expiration time for the token. Typically it is the current time.
  NominalDiffTime ->
  -- | The token
  Text ->
  -- | Issuer
  Text ->
  -- | The subject
  Maybe Text
getSubject jwtSecret now token issuer = do
  jwt <- JWT.decodeAndVerifySignature (toVerify (JWT.hmacSecret jwtSecret)) token
  case hasDateExpired (JWT.exp (JWT.claims jwt)) (numericDate now) of
    Just False -> case isCorrectIssuer (stringOrURIToText <$> JWT.iss (JWT.claims jwt)) issuer of
      Just True -> stringOrURIToText <$> JWT.sub (JWT.claims jwt)
      _ -> Nothing
    _ -> Nothing
  where
    -- \| Determines if it is the correct issuer
    isCorrectIssuer :: Maybe Text -> Text -> Maybe Bool
    isCorrectIssuer isu uri = (==) <$> isu <*> Just uri

    -- \| Determines if a numeric date has expired
    hasDateExpired ::
      Maybe JWT.NumericDate -> -- \^The expiration time
      Maybe JWT.NumericDate -> -- \^The current time
      Maybe Bool -- \^If the time has expired
    hasDateExpired exptime currtime = (<) <$> exptime <*> currtime