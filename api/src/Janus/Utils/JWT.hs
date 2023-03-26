{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Janus.Utils.JWT
-- Description : JSON Web Token functions
-- Copyright   : (c) Tomas Stenlund, 2023
-- License     : GNU AFFERO GENERAL PUBLIC LICENSE
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains functions to create and verify JSON Web Token
-- and to return with the tokens subject
module Janus.Utils.JWT
  ( createToken,
    getSubject,
  )
where

import           Data.Text       (Text, pack, unpack)
import           Data.Time.Clock (NominalDiffTime)
import           Data.UUID
import           Web.JWT         as JWT (Algorithm (HS256),
                                         JOSEHeader (alg, typ),
                                         JWTClaimsSet (exp, iat, iss, sub),
                                         NumericDate, claims,
                                         decodeAndVerifySignature, encodeSigned,
                                         hmacSecret, numericDate, stringOrURI,
                                         stringOrURIToText, toVerify)

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
  UUID ->
  -- | The token
  Text
createToken jwtSecret ndt len issuer subject =
  encodeSigned
    (JWT.hmacSecret jwtSecret)
    mempty {typ = Just "JWT", alg = Just HS256}
    mempty
      { JWT.iat = numericDate ndt,
        JWT.exp = numericDate (ndt + fromIntegral len),
        JWT.sub = stringOrURI (pack $ toString subject),
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
  Maybe UUID
getSubject jwtSecret now token issuer = do
  jwt <- JWT.decodeAndVerifySignature (toVerify (JWT.hmacSecret jwtSecret)) token
  case hasDateExpired (JWT.exp (JWT.claims jwt)) (numericDate now) of
    Just False -> case isCorrectIssuer (stringOrURIToText <$> JWT.iss (JWT.claims jwt)) issuer of
      Just True -> (fromString . unpack . stringOrURIToText) =<< JWT.sub (JWT.claims jwt)
      _         -> Nothing
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
