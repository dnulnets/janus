-- |Language support for the login page.
module Janus.Lang.Users ( i18n, Phrases(..) ) where

import Janus.Lang.I18n
import Data.Tuple (Tuple(Tuple))
import Janus.Lang.Message as MSG
import Record (merge)

type Phrases = (
  active::String, 
  email::String,
  key::String,
  title::String,
  username::String )

i18n::I18n Phrases
i18n = createI18n [Tuple "en-US" eng, Tuple "en-GB" eng, Tuple "sv-SE" swe] (Tuple "en-US" eng)

eng :: Dictionary Phrases
eng = merge
  { active: "Active"
  , email: "Email"
  , key: "UUID"
  , username: "Username"
  , title: "User administration"
  } MSG.eng

swe :: Dictionary Phrases
swe = merge { active: "Aktiv"
  , email: "Email"
  , key: "UUID"
  , username: "Användarnamn"
  , title: "Användaradministration"
  } MSG.swe
