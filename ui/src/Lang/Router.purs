-- |The language support for the router component.
module Janus.Lang.Router (Labels(..), translator) where

import Prelude
import Simple.I18n.Translation (Translation, fromRecord)
import Simple.I18n.Translator (Translator, createTranslator, setLang)
import Type.Proxy (Proxy(..))
import Record.Extra (type (:::), SNil)

-- Symbols should be in alphabetic order.
type Labels = ( "tbd" ::: SNil )

translator :: String -> Translator Labels
translator country =
  ( createTranslator
      (Proxy :: _ "en")
      { en, se }
  ) # setLang country

en :: Translation Labels
en = fromRecord
  { tbd: "To be decided" }

se :: Translation Labels
se = fromRecord
  { tbd: "Att bestämmas" }
