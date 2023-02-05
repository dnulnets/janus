module Janus.Component.HTML.Menu (menu) where

import Prelude
import Data.Maybe (Maybe)

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA

import Janus.Component.HTML.Utils (css, prop, safeHref)
import Janus.Data.Profile (Profile)
import Janus.Data.Route (Route(..))

-- | Creates the html for the menu bar that is used at the top of the application user interface.
menu :: forall i p. Maybe Profile -> Route -> HH.HTML i p
menu _currentUser _route =

  HH.nav [css "navbar navbar-expand-md navbar-light fixed-top bg-light", prop "role" "navigation", HP.id "j-navbar-top"]
  [
    HH.div [css "container-fluid"]
    [
      HH.a [css "navbar-brand", HP.href "#"] [HH.img [HP.src "/static/logo.svg", HP.height 40]],
      HH.button [css "navbar-toggler", HP.type_ HP.ButtonButton, prop "data-bs-toggle" "collapse", 
        prop "data-bs-target" "#j-navbar-collapse", HPA.controls "j-navbar-collapse", HPA.expanded "false",
        HPA.label "Toggle navigation"] [HH.span [css "navbar-toggler-icon"][]],
      HH.div [css "collapse navbar-collapse", HP.id "j-navbar-collapse"]
      [
        HH.ul [css "navbar-nav me-auto mb-2 mb-md-0"]
        [
          navItemDropdown "j-drop1" "Dropdown1" [navItem Home [HH.text "Home"]],
          navItemDropdown "j-drop2" "Dropdown2" [navItem Dashboard [HH.text "Dashboard"]]
        ],
        span "Product:" "SMP",
        span "Team:" "Fragglarna"
      ],
      search
    ]
  ]

  where

  navItemDropdown id s html =
    HH.li [css "nav-item dropdown"]
    [
      HH.a [css "nav-link dropdown-toggle", HP.href "#", HP.id id, prop "role" "button", prop "data-bs-toggle" "dropdown"] [HH.text s],
      HH.ul [css "dropdown-menu dropdown-menu-light"] html
    ]

  navItem r html =
    HH.li
      []
      [ HH.a
          [ css $ "dropdown-item" -- | <> guard (route == r) " active"
          , safeHref r
          ]
          html
      ]

  search = HH.form 
    [css "d-flex", prop "role" "search", HP.action "/search", HP.method HP.GET]
    [HH.input [css "form-control me-2", HP.type_ HP.InputSearch, HP.name "what", HP.placeholder "Search", HPA.label "Search"]
     ]

  span s t = HH.span [css "navbar-text pe-3"] [HH.span [css "fw-bold"] [HH.text s], HH.text t]