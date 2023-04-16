-- |The language support for the login form.
module Janus.Lang.Component.Table (translator, Labels (..)) where

import Prelude
import Simple.I18n.Translation (Translation, fromRecord)
import Simple.I18n.Translator (Translator, createTranslator, setLang)
import Type.Proxy (Proxy(..))
import Record.Extra (type (:::), SNil)

-- Symbols should be in alphabetic order.
type Labels =
    ( "create"
  ::: "delete"
  ::: "edit"
  ::: "next"
  ::: "objects"
  ::: "of"
  ::: "previous"
  ::: "showobject"
  ::: "showpage"
  ::: "shows"
  ::: "to"
  ::: SNil
    )

translator :: String -> Translator Labels
translator country =
  (createTranslator
    (Proxy :: _ "en")
    { en, se }) # setLang country

en :: Translation Labels
en = fromRecord
  { shows: "Showing "
  , objects: " objects per page"
  , create: "Create"
  , of: " of "
  , to: "-"
  , showobject: "Showing object "
  , showpage: "Showing page "
  , previous: "Previous"
  , next: "Next"
  , edit: "Edit"
  , delete: "Delete"
  }

se :: Translation Labels
se = fromRecord
  { shows: "Visar "
  , objects: " objekt per sida"
  , create: "Ny"
  , of: " av "
  , to: "-"
  , showobject: "Visar objekt "
  , showpage: "Visar sida "
  , previous: "Previous"
  , next: "Next"
  , edit: "Ändra"
  , delete: "Ta bort"
  }
