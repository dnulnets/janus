-- |The language support for the login form.
module Janus.Lang.Component.Table (translator, Labels (..)) where

import Prelude
import Simple.I18n.Translation (Translation, fromRecord)
import Simple.I18n.Translator (Translator, createTranslator, setLang)
import Type.Proxy (Proxy(..))
import Record.Extra (type (:::), SNil)

-- Symbols should be in alphabetic order.
type Labels =
    ( "invalid"
  ::: "pwd"
  ::: "uname"
  ::: SNil
    )

translator :: String -> Translator Labels
translator country =
  (createTranslator
    (Proxy :: _ "en")
    { en, se }) # setLang country

en :: Translation Labels
en = fromRecord
  { pwd: "Password"
  , uname: "Username"
  , invalid: "Username or password is invalid"
  }

se :: Translation Labels
se = fromRecord
  { pwd: "Lösenord"
  , uname: "Användarnamn"
  , invalid: "Användarnamn eller lösenord är felaktigt"
  }
