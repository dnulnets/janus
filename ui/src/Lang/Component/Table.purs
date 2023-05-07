-- |The language support for the login form.
module Janus.Lang.Component.Table (i18n, Phrases(..)) where

import Data.Tuple (Tuple(Tuple))
import Data.Map(Map, empty)
import Janus.Lang.I18n (Dictionary, I18n, createI18n)
import Janus.Lang.Message as MSG
import Record (merge)

-- Symbols should be in alphabetic order.
type Phrases = ( action::String,
  create::String,
  delete::String,
  edit::String,
  next::String,
  objects::String,
  of::String,
  previous::String,
  showobject::String,
  showpage::String,
  shows::String,
  to::String)

i18n::I18n Phrases
i18n = createI18n [Tuple "en-US" eng, Tuple "en-GB" eng, Tuple "sv-SE" swe] (Tuple "en-US" eng)

eng :: Dictionary Phrases
eng = merge
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
  , action: "Action"
  , messages: empty::(Map String String)
  } MSG.eng

swe :: Dictionary Phrases
swe = merge
  { shows: "Visar "
  , objects: " objekt per sida"
  , create: "Ny"
  , of: " av "
  , to: "-"
  , showobject: "Visar objekt "
  , showpage: "Visar sida "
  , previous: "Föregående"
  , next: "Nästa"
  , edit: "Ändra"
  , delete: "Ta bort"
  , action: "Åtgärd"
  , messages: empty::(Map String String)
  } MSG.swe
