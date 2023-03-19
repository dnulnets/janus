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
import Janus.Api.Request (writeCountry)
import Janus.Capability.Navigate (class Navigate, navigate)
import Janus.Capability.Resource.User (class ManageUser, loginUser)
import Janus.Capability.I18n
import Janus.Component.HTML.Fragments (full)
import Janus.Component.HTML.Utils (css, whenElem, prop)
import Janus.Data.Route (Route(..))
import Janus.Data.Username (Username)
import Janus.Form.Field as Field
import Janus.Form.Login as L
import Janus.Form.Validation (FormError)
import Janus.Form.Validation as V
import Janus.Lang.Login
import Type.Proxy (Proxy(..))
import Simple.I18n.Translation (Translation, fromRecord)
import Simple.I18n.Translator (Translator, createTranslator, label, setLang, translate)
import Halogen.Store.Monad (class MonadStore, StoreT, getStore, runStoreT, updateStore)
import Janus.Store as Store

type Input = { redirect :: Boolean
             , country :: String }

type State = { redirect :: Boolean
                , i18n ::  Translator Labels}

type ChildSlots = ( login :: L.Slot )

data Action = Country String

component
  :: forall q o m
   . MonadAff m
  => Navigate m
  => ManageUser m
  => MonadStore Store.Action Store.Store m
  => I18n m
  => H.Component q Input o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    {handleAction = handleAction}
  }
  where

    initialState r = { redirect: r.redirect, i18n : translator r.country}

    handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
    handleAction = case _ of
    
      Country c -> do
        H.liftEffect $ log $ "Login.Country " <> c
        H.liftEffect $ writeCountry c
        updateStore (Store.Country c)
        H.modify_ (\state -> state { i18n = state.i18n # setLang c})

    render :: State -> H.ComponentHTML Action ChildSlots m
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
                                        HH.text (state.i18n # translate (label :: _ "country"))
                                    ],
                                    HH.ul [css "dropdown-menu", HPA.labelledBy "j-dropdownlink"]
                                    [
                                        HH.li_ [HH.a [css "dropdown-item", HE.onClick (\_->Country "se")][HH.text (state.i18n # translate (label :: _ "se_country"))]],
                                        HH.li_ [HH.a [css "dropdown-item", HE.onClick (\_->Country "us")][HH.text (state.i18n # translate (label :: _ "us_country"))]],
                                        HH.li_ [HH.a [css "dropdown-item", HE.onClick (\_->Country "gb")][HH.text (state.i18n # translate (label :: _ "gb_country"))]]
                                    ]
                                ]
                            ]
                        ],
                        HH.slot_ (Proxy :: _ "login") unit L.component { redirect: state.redirect }
                    ]

                ]
            ]
        ]
