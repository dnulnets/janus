module Janus.Lang.Login (translator, Labels (..)) where

import Prelude
import Simple.I18n.Translation (Translation, fromRecord)
import Simple.I18n.Translator (Translator, createTranslator, label, setLang, translate)
import Type.Proxy (Proxy(..))
import Record.Extra (type (:::), SNil)

-- Symbols should be in alphabetic order.
type Labels =
    ( "gb_country"
  ::: "se_country"
  ::: "us_country"
  ::: SNil
    )

translator :: String -> Translator Labels
translator country =
  (createTranslator
    (Proxy :: _ "en")
    { en, se }) # setLang country

en :: Translation Labels
en = fromRecord
  { se_country: "Sweden"
  , us_country: "USA"
  , gb_country: "Great Britain"
  }

se :: Translation Labels
se = fromRecord
  { se_country: "Sverige"
  , us_country: "USA"
  , gb_country: "Storbritannien"
  }
