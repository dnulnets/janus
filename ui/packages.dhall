let upstream = https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20221117/packages.dhall
        sha256:02b87b86790df3a233d475623debc8861241df57c04a641c9e5cb45b64a8b0e7

{-       https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220504/packages.dhall
        sha256:fd37736ecaa24491c907af6a6422156417f21fbf25763de19f65bd641e8340d3
-}

let overrides = {=}

let additions =
      { simple-i18n =
        { dependencies =
          [ "foreign-object"
            , "maybe"
            , "prelude"
            , "record"
            , "record-extra"
            , "typelevel-prelude"
            , "unsafe-coerce"
            ]
        , repo = "https://github.com/oreshinya/purescript-simple-i18n.git"
        , version = "v2.0.1"
        }
      }

in  upstream // overrides // additions