-- |Language support for the login page.
module Janus.Lang.Message (eng, swe) where

import Data.Map (fromFoldable)
import Data.Tuple (Tuple(Tuple))
import Janus.Lang.I18n (Dictionary)

eng :: Dictionary ()
eng = {
  messages: fromFoldable [ 
      Tuple "JAN001" "Unknown error"
    , Tuple "JAN002" "Communication failure to the server"
    , Tuple "JAN003" "Server error"
    , Tuple "USR001" "Username already exists"
  ]
  , fallback: "Unknown error message"
  }

swe :: Dictionary ()
swe = {
  messages: fromFoldable [
      Tuple "JAN001" "Okänt fel"
    , Tuple "JAN002" "Kommunikationsproblem med servern"
    , Tuple "JAN003" "Systemfel"
    , Tuple "USR001" "Användarnamnet finns redan"
  ]
  , fallback: "Okänt felmeddelande"
  }
