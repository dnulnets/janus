-- |The language support for the validation of the input components.
module Janus.Lang.Validation (i18n, Phrases(..)) where

import Janus.Lang.I18n

import Data.Map (Map, empty)
import Data.Tuple (Tuple(Tuple))
import Janus.Lang.Message as MSG
import Record (merge)

-- Symbols should be in alphabetic order.
type Phrases = (
  invalidEmail::String,
  invalidUsername::String,
  required::String,
  tooLong::String,
  tooShort::String)

i18n::I18n Phrases
i18n = createI18n [Tuple "en-US" eng, Tuple "en-GB" eng, Tuple "sv-SE" swe] (Tuple "en-US" eng)

-- eng :: Dictionary Phrases
eng::Dictionary Phrases
eng = merge
  { invalidEmail: "Invalid email address"
  , invalidUsername: "Invalid username"
  , required: "This field is required"
  , tooLong: "Too many characters entered"
  , tooShort: "Not enough characters"
  , messages: empty::(Map String String)
  } MSG.eng

-- swe :: Dictionary Phrases
swe::Dictionary Phrases
swe = merge
  { invalidEmail: "Ej giltig epostadress"
  , invalidUsername: "Ej giltigt användarnamn"
  , required: "Detta fält är obligatoriskt"
  , tooLong: "För många tecken"
  , tooShort: "Inte tillräckligt många tecken"
  , messages: empty::(Map String String)
  } MSG.eng
