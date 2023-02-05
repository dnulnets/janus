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
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Routing.Duplex as RD
import Routing.Hash (getHash)
import Type.Proxy (Proxy(..))

import Janus.Capability.LogMessages (class LogMessages)
import Janus.Capability.Navigate (class Navigate, navigate)
import Janus.Capability.Now (class Now)
import Janus.Capability.Resource.User (class ManageUser)
import Janus.Component.Utils (OpaqueSlot)
import Janus.Data.Profile (Profile)
import Janus.Data.Route (Route(..), routeCodec)
import Janus.Data.Username (toString)
import Janus.Page.Home as Home
import Janus.Page.Login as Login
import Janus.Page.Dashboard as Dashboard
import Janus.Store as Store

-- |The router page query messages used for navigation.
data Query a = Navigate Route a

-- |The state of the application.
type State =
  { route :: Maybe Route
  , currentUser :: Maybe Profile
  }

-- |The activities for the router page.
data Action
  = Initialize
  | Receive (Connected (Maybe Profile) Unit)

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
component = connect (selectEq _.currentUser) $ H.mkComponent
  { initialState: \{ context: currentUser } -> { route: Nothing, currentUser }
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

    Receive { context: currentUser } -> do
      H.liftEffect $ log $ "Router.Receive " <> show (toString <$> (_.username <$> currentUser))
      H.modify_ _ { currentUser = currentUser }

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
  authorize :: Maybe Profile -> H.ComponentHTML Action ChildSlots m -> H.ComponentHTML Action ChildSlots m
  authorize mbProfile html = case mbProfile of
    Nothing ->
      HH.slot (Proxy :: _ "login") unit Login.component { redirect: false } absurd
    Just _ ->
      html

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { route, currentUser } = case route of
    Just r -> case r of
      Home -> authorize currentUser do
        HH.slot_ (Proxy :: _ "home") unit Home.component unit
      Dashboard -> authorize currentUser do
        HH.slot_ (Proxy :: _ "dashboard") unit Dashboard.component unit
      Login ->
        HH.slot_ (Proxy :: _ "login") unit Login.component { redirect: true }
    Nothing ->
      HH.div_ [ HH.text "Oh no! That page wasn't found." ]
