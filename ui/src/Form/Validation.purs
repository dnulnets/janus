module Janus.Form.Validation where

import Prelude

import Janus.Data.Email (Email(..))
import Janus.Data.Username (Username)
import Janus.Data.Username as Username
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

required :: forall a. Eq a => Monoid a => a -> Either FormError a
required = check (_ /= mempty) Required

minLength :: Int -> String -> Either FormError String
minLength n = check (\str -> String.length str > n) TooShort

maxLength :: Int -> String -> Either FormError String
maxLength n = check (\str -> String.length str <= n) TooLong

emailFormat :: String -> Either FormError Email
emailFormat = map Email <<< check (String.contains (String.Pattern "@")) InvalidEmail

usernameFormat :: String -> Either FormError Username
usernameFormat = note InvalidUsername <<< Username.parse

check :: forall a. (a -> Boolean) -> FormError -> a -> Either FormError a
check f err a
  | f a = Right a
  | otherwise = Left err

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
