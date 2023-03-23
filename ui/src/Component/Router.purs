-- |This module contains the main router component that based on the current hash displays the
-- |correct page.
module Janus.Component.Router where

import Prelude

import Data.Either (hush)
import Data.Foldable (elem)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq, selectAll)
import Janus.Capability.LogMessages (class LogMessages)
import Janus.Capability.Navigate (class Navigate, navigate)
import Janus.Capability.Now (class Now)
import Janus.Capability.I18n (class I18n, country)
import Janus.Capability.Resource.User (class ManageUser)
import Janus.Component.Utils (OpaqueSlot)
import Janus.Component.HTML.Utils (css, prop, safeHref)
import Janus.Component.HTML.Fragments (main, full)
import Janus.Data.Profile (Profile)
import Janus.Data.Route (Route(..), routeCodec)
import Janus.Data.Username (toString)
import Janus.Page.Dashboard as Dashboard
import Janus.Page.Home as Home
import Janus.Page.Login as Login
import Janus.Store as Store
import Routing.Duplex as RD
import Routing.Hash (getHash)
import Type.Proxy (Proxy(..))
import Janus.Component.HTML.Fragments (main)

-- |The router page query messages used for navigation.
data Query a = Navigate Route a

-- |The state of the application.
type State =
  { route :: Maybe Route
  , currentUser :: Maybe Profile
  , country :: String
  }

-- |The actions for the router page.
data Action
  = Initialize
  | Receive (Connected (Store.Store) Unit)

-- |The pages that can be displayed in the router.
type ChildSlots =
  ( home :: OpaqueSlot Unit
  , login :: OpaqueSlot Unit
  , dashboard :: OpaqueSlot Unit
  )

-- |The router component.
component
  :: forall m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Now m
  => LogMessages m
  => Navigate m
  => ManageUser m
  => H.Component Query Unit Void m
component = connect selectAll $ H.mkComponent
  { initialState: \{ context: ctx } -> { route: Nothing, currentUser: ctx.currentUser, country: ctx.country }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleQuery = handleQuery
      , handleAction = handleAction
      , receive = Just <<< Receive
      , initialize = Just Initialize
      }
  }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      H.liftEffect $ log "Router.Initialize"
      -- first we'll get the route the user landed on
      initialRoute <- hush <<< (RD.parse routeCodec) <$> liftEffect getHash
      -- then we'll navigate to the new route (also setting the hash)
      navigate $ fromMaybe Home initialRoute

    Receive { context: ctx } -> do
      H.liftEffect $ log $ "Router.Receive User = " <> show (toString <$> (_.username <$> (ctx.currentUser)))
      H.liftEffect $ log $ "Router.Receive Country = " <> ctx.country
      H.modify_ _ { currentUser = ctx.currentUser, country = ctx.country }

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      { route, currentUser } <- H.get
      H.liftEffect $ log $ "Router.Navigate " <> show (toString <$> (_.username <$> currentUser))
      -- don't re-render unnecessarily if the route is unchanged
      when (route /= Just dest) do
        -- don't change routes if there is a logged-in user trying to access
        -- a route only meant to be accessible to a not-logged-in session
        case (isJust currentUser && dest `elem` [ ]) of
          false -> H.modify_ _ { route = Just dest }
          _ -> pure unit
      pure (Just a)

  -- Display the login page instead of the expected page if there is no current user; a simple
  -- way to restrict access.
  authorize :: State -> H.ComponentHTML Action ChildSlots m -> H.ComponentHTML Action ChildSlots m
  authorize { currentUser: currentUser, country: country } html = case currentUser of
    Nothing -> do
      HH.slot (Proxy :: _ "login") unit Login.component { redirect: false, country: country} absurd
    Just _ ->
      html

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state@{ route: route, currentUser: currentUser, country: country } = case route of
    Just r -> case r of
      Home -> authorize state do
        HH.div [][menu currentUser Home, main $ HH.slot_ (Proxy :: _ "home") unit Home.component Home.Unit]
      Dashboard -> authorize state do
        HH.div [][menu currentUser Dashboard, main $ HH.slot_ (Proxy :: _ "dashboard") unit Dashboard.component unit]
      Login -> do
        HH.slot_ (Proxy :: _ "login") unit Login.component { redirect: true, country: country }
    Nothing ->
      full $ HH.div_ [ HH.text "Oh no! That page wasn't found." ]

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