-- |The language support for the login form.
module Janus.Lang.Form.User (translator, Labels (..)) where

import Prelude
import Simple.I18n.Translation (Translation, fromRecord)
import Simple.I18n.Translator (Translator, createTranslator, setLang)
import Type.Proxy (Proxy(..))
import Record.Extra (type (:::), SNil)

-- Symbols should be in alphabetic order.
type Labels =
    ( "active"
  ::: "email"
  ::: "invalid"
  ::: "key"
  ::: "password"
  ::: "username"
  ::: SNil
    )

translator :: String -> Translator Labels
translator country =
  (createTranslator
    (Proxy :: _ "en")
    { en, se }) # setLang country

en :: Translation Labels
en = fromRecord
  { password: "Password"
  , username: "Username"
  , invalid: "Username or password is invalid"
  , email: "Email"
  , active: "Active"
  , key: "UUID"
  }

se :: Translation Labels
se = fromRecord
  { password: "Lösenord"
  , username: "Användarnamn"
  , invalid: "Användarnamn eller lösenord är felaktigt"
  , email: "Email"
  , active: "Active"
  , key: "UUID"
  }
