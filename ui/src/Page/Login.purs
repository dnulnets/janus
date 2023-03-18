module Janus.Page.Login
  ( component )
  where

import Janus.Capability.Resource.User
import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Janus.Capability.Navigate (class Navigate, navigate)
import Janus.Capability.Resource.User (class ManageUser, loginUser)
import Janus.Component.HTML.Fragments (full)
import Janus.Component.HTML.Utils (css, whenElem, prop)
import Janus.Data.Route (Route(..))
import Janus.Data.Username (Username)
import Janus.Form.Field as Field
import Janus.Form.Login as L
import Janus.Form.Validation (FormError)
import Janus.Form.Validation as V
import Type.Proxy (Proxy(..))

type Input = { redirect :: Boolean }

type State = { redirect :: Boolean }

type ChildSlots = ( login :: L.Slot )

component
  :: forall q o m
   . MonadAff m
  => Navigate m
  => ManageUser m
  => H.Component q Input o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval 
  }
  where

    initialState r = r

    render :: forall a . State -> H.ComponentHTML a ChildSlots m
    render state = 
        full $ HH.section [css "vh-100"]
        [
            HH.div [css "container-fluid h-custom workarea"]
            [
                HH.div [css "row d-flex justify-content-center align-items-center h-100"]
                [
                    HH.div [css "col-md-9 col-lg-6 col-xl-5"]
                    [
                        HH.img [css "img-fluid", HP.alt "Janus logo", HP.src "/static/logo.png"]
                    ],
                    HH.div [css "col-md-8 col-lg-6 col-xl-4 offset-xl-1", HP.id "j-login"]
                    [
                        HH.div [css "row"]
                        [
                            HH.div [css "col"]
                            [
                                HH.h1 [][HH.b [][HH.text "Janus"]]
                            ],
                            HH.div [css "col d-flex align-items-center justify-content-end"]
                            [
                                HH.div [css "dropdown"]
                                [
                                    HH.a [css "btn btn-primary dropdown-toggle", HP.id "j-dropdownlink", HP.href "#", prop "role" "button",
                                        prop "data-bs-toggle" "dropdown", HPA.expanded "false"]
                                    [
                                        HH.text "Country"
                                    ],
                                    HH.ul [css "dropdown-menu", HPA.labelledBy "j-dropdownlink"]
                                    [
                                        HH.li_ [HH.a [css "dropdown-item", HP.href "#"][HH.text "Swedena"]],
                                        HH.li_ [HH.a [css "dropdown-item", HP.href "#"][HH.text "USA"]],
                                        HH.li_ [HH.a [css "dropdown-item", HP.href "#"][HH.text "Great Britain"]]
                                    ]
                                ]
                            ]
                        ],
                        HH.slot_ (Proxy :: _ "login") unit L.component state
                    ]

                ]
            ]
        ]
