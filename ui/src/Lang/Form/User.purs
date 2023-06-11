-- |The language support for the login form.
module Janus.Lang.Form.User (i18n, Phrases(..)) where

import Data.Tuple (Tuple(Tuple))
import Janus.Lang.I18n (Dictionary, I18n, createI18n)
import Record (merge)
import Janus.Lang.Message as MSG

-- Symbols should be in alphabetic order.
type Phrases = ( active::String,
      cancel::String,
      create::String,
      delete::String,
      email::String,
      invalid::String,
      key::String,
      password::String,
      save::String,
      username::String,
      roles::String)


i18n::I18n Phrases
i18n = createI18n [Tuple "en-US" eng, Tuple "en-GB" eng, Tuple "sv-SE" swe] (Tuple "en-US" eng)

eng :: Dictionary Phrases
eng = merge
  { password: "Password"
  , username: "Username"
  , invalid: "Username or password is invalid"
  , email: "Email"
  , active: "Active"
  , key: "UUID"
  , create: "Create"
  , save: "Save"
  , cancel: "Cancel"
  , delete: "Delete"
  , roles: "Roles"
  } MSG.eng

swe :: Dictionary Phrases
swe = merge
  { password: "Lösenord"
  , username: "Användarnamn"
  , invalid: "Användarnamn eller lösenord är felaktigt"
  , email: "Email"
  , active: "Active"
  , key: "UUID"
  , create: "Skapa"
  , save: "Spara"
  , cancel: "Avbryt"
  , delete: "Delete"
  , roles: "Roles"
  } MSG.swe
