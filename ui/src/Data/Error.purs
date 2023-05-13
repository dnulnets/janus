module Janus.Data.Error where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Janus.Lang.I18n (I18n, setLocale, message)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR

type Error = {
  code::String
  , status::Maybe Int
  , extra:: Maybe String }

errorCodec :: JsonCodec Error
errorCodec =
  CAR.object "Error"
    { code: CA.string,
      status: CAR.optional CA.int,
      extra: CAR.optional CA.string
    }

flash::forall r . I18n r -> Error -> String
flash i18n {code:code, status:status, extra:extra} = message i18n code
  <> 
  case status of
    Just s -> ", " <> ", RC=" <> show s
    Nothing -> ""
  <>
  case extra of
    Just e -> ", EXTRA=" <> e
    Nothing -> ""
