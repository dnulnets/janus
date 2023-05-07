-- |Language support for the login page.
module Janus.Lang.Login (i18n, Phrases(..)) where

import Data.Tuple (Tuple(Tuple))
import Janus.Lang.I18n (Dictionary, I18n, createI18n)
import Janus.Lang.Message as MSG
import Record (merge)

-- Symbols should be in alphabetic order.
type Phrases =
    ( country::String
  , gb_country::String
  , se_country::String
  , us_country::String)

i18n::I18n Phrases
i18n = createI18n [Tuple "en-US" eng, Tuple "en-GB" eng, Tuple "sv-SE" swe] (Tuple "en-US" eng)

eng :: Dictionary Phrases
eng = merge { country: "Country"
  , se_country: "Sweden"
  , us_country: "USA"
  , gb_country: "Great Britain"
  } MSG.eng

swe :: Dictionary Phrases
swe = merge { country: "Land"
  , se_country: "Sverige"
  , us_country: "USA"
  , gb_country: "Storbritannien"
  } MSG.swe
