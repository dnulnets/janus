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
    , Tuple "JAN004" "You are not authorized"
    , Tuple "JAN005" "Malformed response from server"
    , Tuple "USR001" "Username already exists"
    , Tuple "USR002" "Unable to find new user"
  ]
  , fallback: "Unknown error message"
  }

swe :: Dictionary ()
swe = {
  messages: fromFoldable [
      Tuple "JAN001" "Okänt fel"
    , Tuple "JAN002" "Kommunikationsproblem med servern"
    , Tuple "JAN003" "Systemfel"
    , Tuple "JAN004" "Du är inte behörig"
    , Tuple "JAN005" "Felformaterat svar från servern"
    , Tuple "USR001" "Användarnamnet finns redan"
    , Tuple "USR002" "Kan ej hitta den nya användaren"
  ]
  , fallback: "Okänt felmeddelande"
  }
