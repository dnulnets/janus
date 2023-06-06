{ name = "janus"
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-web"
  , "argonaut-core"
  , "arrays"
  , "bifunctors"
  , "codec"
  , "codec-argonaut"
  , "console"
  , "datetime"
  , "dom-indexed"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "formatters"
  , "halogen"
  , "halogen-formless"
  , "halogen-store"
  , "http-methods"
  , "maybe"
  , "newtype"
  , "now"
  , "ordered-collections"
  , "prelude"
  , "profunctor"
  , "profunctor-lenses"
  , "record"
  , "remotedata"
  , "routing"
  , "routing-duplex"
  , "safe-coerce"
  , "strings"
  , "transformers"
  , "tuples"
  , "web-events"
  , "web-html"
  , "web-promise"
  , "web-storage"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
