module Janus.Lang.Validation (translator, Labels (..)) where

import Prelude
import Simple.I18n.Translation (Translation, fromRecord)
import Simple.I18n.Translator (Translator, createTranslator, setLang)
import Type.Proxy (Proxy(..))
import Record.Extra (type (:::), SNil)

-- Symbols should be in alphabetic order.
type Labels =
    ( "invalidEmail"
  ::: "invalidUsername"
  ::: "required"
  ::: "tooLong"
  ::: "tooShort"
  ::: SNil
    )

translator :: String -> Translator Labels
translator country =
  (createTranslator
    (Proxy :: _ "en")
    { en, se }) # setLang country

en :: Translation Labels
en = fromRecord
  { invalidEmail: "Invalid email address"
  , invalidUsername: "Invalid username"
  , required: "This field is required"
  , tooLong: "Too many characters entered"
  , tooShort: "Not enough characters"
  }

se :: Translation Labels
se = fromRecord
  { invalidEmail: "Ej giltig epostadress"
  , invalidUsername: "Ej giltigt användarnamn"
  , required: "Detta fält är obligatoriskt"
  , tooLong: "För många tecken"
  , tooShort: "Inte tillräckligt många tecken"
  }
