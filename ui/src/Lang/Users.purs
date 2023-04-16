-- |Language support for the login page.
module Janus.Lang.Users
  ( Labels(..)
  , en
  , translator
  ) where

import Prelude
import Simple.I18n.Translation (Translation, fromRecord)
import Simple.I18n.Translator (Translator, createTranslator, setLang)
import Type.Proxy (Proxy(..))
import Record.Extra (type (:::), SNil)

-- Symbols should be in alphabetic order.
type Labels =
    ( "title"
  ::: SNil
    )

translator :: String -> Translator Labels
translator country =
  (createTranslator
    (Proxy :: _ "en")
    { en, se }) # setLang country

en :: Translation Labels
en = fromRecord
  { title: "User administration"
  }

se :: Translation Labels
se = fromRecord
  { title: "Användaradministration"
  }
