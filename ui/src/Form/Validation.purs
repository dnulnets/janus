-- |This module contains various functions for validating input components.
module Janus.Form.Validation where

import Prelude

import Janus.Data.Email (Email(..))
import Janus.Data.Username (Username)
import Janus.Data.Username as Username
import Janus.Data.UUID (UUID(..))
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Data.String as String
import Janus.Lang.Validation as Validation
import Simple.I18n.Translator ( translate, label)

data FormError
  = Required
  | TooShort
  | TooLong
  | InvalidEmail
  | InvalidUsername

errorToString :: FormError -> String -> String
errorToString fe country = case fe of
  Required -> (Validation.translator country) # translate (label :: _ "required")
  TooShort -> (Validation.translator country) # translate (label :: _ "tooShort")
  TooLong -> (Validation.translator country) # translate (label :: _ "tooLong")
  InvalidEmail -> (Validation.translator country) # translate (label :: _ "invalidEmail")
  InvalidUsername -> (Validation.translator country) # translate (label :: _ "invalidUsername")

-- |The input is required.
required :: forall a. Eq a => Monoid a => a -> Either FormError a
required = check (_ /= mempty) Required

-- |The input must contains at lease this number of characters.
minLength :: Int -> String -> Either FormError String
minLength n = check (\str -> String.length str > n) TooShort

-- |The input cannot exceed this number of characters. 
maxLength :: Int -> String -> Either FormError String
maxLength n = check (\str -> String.length str <= n) TooLong

-- |The input must be an email.
emailFormat :: String -> Either FormError Email
emailFormat = map Email <<< check (String.contains (String.Pattern "@")) InvalidEmail

-- |The input must be a username.
usernameFormat :: String -> Either FormError Username
usernameFormat = note InvalidUsername <<< Username.parse

uuidFormat :: String -> Either FormError UUID
uuidFormat s = Right $ UUID s

-- |Handles a "generic" check for an input.
check :: forall a. (a -> Boolean) -> FormError -> a -> Either FormError a
check f err a
  | f a = Right a
  | otherwise = Left err

-- |Converts a strict validation to an optional one.
toOptional
  :: forall a b
   . Monoid a
  => Eq a
  => (a -> Either FormError b)
  -> (a -> Either FormError (Maybe b))
toOptional k = \value ->
  if value == mempty then
    Right Nothing
  else
    map Just $ k value
