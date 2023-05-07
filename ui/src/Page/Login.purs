-- |This module contains the login page.
module Janus.Page.Login
  ( Input
  , component
  ) where

import Prelude
import Janus.Capability.Resource.User (class ManageUser)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Janus.Api.Request (writeLocale)
import Janus.Capability.Navigate (class Navigate)
import Janus.Component.HTML.Fragments (full)
import Janus.Component.HTML.Utils (css, prop)
import Janus.Form.Login as L
import Janus.Lang.Login (Phrases(..), i18n)
import Type.Proxy (Proxy(..))
import Janus.Lang.I18n (I18n, Dictionary, setLocale)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Janus.Store as Store

type Input =
  { redirect :: Boolean
  , locale :: String
  }

type State =
  { redirect :: Boolean
  , i18n :: I18n Phrases
  }

type ChildSlots = (login :: L.Slot)

data Action = Locale String

component
  :: forall q o m
   . MonadAff m
  => Navigate m
  => ManageUser m
  => MonadStore Store.Action Store.Store m
  => H.Component q Input o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction }
  }
  where

  initialState r = { redirect: r.redirect, i18n: setLocale i18n r.locale }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of

    Locale c -> do
      H.liftEffect $ log $ "Login.Locale " <> c
      H.liftEffect $ writeLocale c
      updateStore (Store.Locale c)
      H.modify_ (\state -> state { i18n = setLocale i18n c })

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { i18n: i18n, redirect: redirect } =
    full $ HH.section [ css "vh-100" ]
      [ HH.div [ css "container-fluid h-custom workarea" ]
          [ HH.div [ css "row d-flex justify-content-center align-items-center h-100" ]
              [ HH.div [ css "col-md-9 col-lg-6 col-xl-5" ]
                  [ HH.img [ css "img-fluid", HP.alt "Janus logo", HP.src "/static/logo.png" ]
                  ]
              , HH.div [ css "col-md-8 col-lg-6 col-xl-4 offset-xl-1", HP.id "j-login" ]
                  [ HH.div [ css "row" ]
                      [ HH.div [ css "col" ]
                          [ HH.h1 [] [ HH.b [] [ HH.text "Janus" ] ]
                          ]
                      , HH.div [ css "col d-flex align-items-center justify-content-end" ]
                          [ HH.div [ css "dropdown" ]
                              [ HH.a [ css "btn btn-primary dropdown-toggle", HP.id "j-dropdownlink", HP.href "#", prop "role" "button", prop "data-bs-toggle" "dropdown", HPA.expanded "false" ]
                                  [ HH.text (i18n.dictionary.country)
                                  ]
                              , HH.ul [ css "dropdown-menu", HPA.labelledBy "j-dropdownlink" ]
                                  [ HH.li_ [ HH.a [ css "dropdown-item", HE.onClick (\_ -> Locale "sv-SE") ] [ HH.text (i18n.dictionary.se_country) ] ]
                                  , HH.li_ [ HH.a [ css "dropdown-item", HE.onClick (\_ -> Locale "en-US") ] [ HH.text (i18n.dictionary.us_country) ] ]
                                  , HH.li_ [ HH.a [ css "dropdown-item", HE.onClick (\_ -> Locale "en-GB") ] [ HH.text (i18n.dictionary.gb_country) ] ]
                                  ]
                              ]
                          ]
                      ]
                  , HH.slot_ (Proxy :: _ "login") unit L.component { redirect: redirect, locale: i18n.locale }
                  ]

              ]
          ]
      ]

