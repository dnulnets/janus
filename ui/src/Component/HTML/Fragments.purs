module Janus.Component.HTML.Fragments (main, full) where

import Halogen.HTML as HH

import Janus.Component.HTML.Utils (css, prop)

-- |The main element for the application, it is used for the pages that have a navigational bar at the top
main :: forall p i. HH.HTML p i -> HH.HTML p i
main html =     HH.main [css "container-fluid j-workarea", prop "role" "main"]
    [
        HH.div [css "bg-light p-2 rounded"]
        [
          html
        ]
    ]

-- |The main element for the application when it has no navigational bar, such as the login page or any other
-- |informational pages.
full :: forall p i. HH.HTML p i -> HH.HTML p i
full html =     HH.main [css "bg-light", prop "role" "main"]
    [ html
    ]