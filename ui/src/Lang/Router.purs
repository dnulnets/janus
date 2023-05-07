-- |The language support for the router component.
module Janus.Lang.Router (i18n, Phrases(..)) where

import Janus.Lang.I18n

import Data.Tuple (Tuple(Tuple))
import Janus.Lang.Message as MSG
import Record (merge)

-- Symbols should be in alphabetic order.
type Phrases = ( 
  admin::String,
  search::String,
  users::String)

i18n::I18n Phrases
i18n = createI18n [Tuple "en-US" eng, Tuple "en-GB" eng, Tuple "sv-SE" swe] (Tuple "en-US" eng)

eng :: Dictionary Phrases
eng = merge
  { admin: "Administration",
    search: "Search",
    users: "Users" } MSG.eng

swe :: Dictionary Phrases
swe = merge
  { admin: "Administration",
    search: "Sök",
    users: "Användare" } MSG.swe
