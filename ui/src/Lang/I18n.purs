-- |The language support for the login form.
module Janus.Lang.I18n where

import Prelude
import Data.Map (Map, fromFoldable, lookup, member)
import Data.Tuple (Tuple, fst, snd)
import Data.Maybe (fromMaybe)

type Dictionary r = {messages::Map String String, fallback::String|r}

type I18n r = {dictionary :: Dictionary r,
  locale:: String,
  dictionaries :: Map String (Dictionary r),
  default :: Tuple String (Dictionary r)}

createI18n::forall r . Array (Tuple String (Dictionary r))->Tuple String (Dictionary r)->I18n r
createI18n al l = {
    dictionary: snd l,
    locale: fst l,
    default: l,
    dictionaries: fromFoldable al
  }

setLocale::forall r . I18n r->String->I18n r
setLocale t l =
  t { locale = locale, dictionary = dictionary }
  where
    dictionary = fromMaybe (snd t.default) $ lookup l t.dictionaries
    locale = if member l t.dictionaries then l else fst t.default

message::forall r . I18n r->String->String
message t k = k <> ": " <> (fromMaybe t.dictionary.fallback $ lookup k t.dictionary.messages)
