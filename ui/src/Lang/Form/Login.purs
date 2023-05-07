-- |The language support for the login form.
module Janus.Lang.Form.Login (i18n, Phrases(..)) where

import Data.Tuple (Tuple(Tuple))
import Janus.Lang.I18n (Dictionary, I18n, createI18n)
import Janus.Lang.Message as MSG
import Record (merge)

-- Symbols should be in alphabetic order.
type Phrases =
    ( invalid::String,
      pwd::String ,
      uname::String,
      login::String)

i18n::I18n Phrases
i18n = createI18n [Tuple "en-US" eng, Tuple "en-GB" eng, Tuple "sv-SE" swe] (Tuple "en-US" eng)

eng :: Dictionary Phrases
eng = merge
  { pwd: "Password"
  , uname: "Username"
  , login: "Log in"
  , invalid: "Username or password is invalid"
  } MSG.eng

swe :: Dictionary Phrases
swe = merge
  { pwd: "Lösenord"
  , login: "Logga in"
  , uname: "Användarnamn"
  , invalid: "Användarnamn eller lösenord är felaktigt"
  } MSG.swe
