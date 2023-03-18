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
  , "const"
  , "datetime"
  , "dom-indexed"
  , "effect"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "formatters"
  , "halogen"
  , "halogen-formless"
  , "halogen-store"
  , "http-methods"
  , "lists"
  , "maybe"
  , "newtype"
  , "now"
  , "ordered-collections"
  , "parallel"
  , "precise-datetime"
  , "prelude"
  , "profunctor"
  , "profunctor-lenses"
  , "record-extra"
  , "remotedata"
  , "routing"
  , "routing-duplex"
  , "safe-coerce"
  , "simple-i18n"
  , "slug"
  , "strings"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "web-events"
  , "web-html"
  , "web-storage"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
