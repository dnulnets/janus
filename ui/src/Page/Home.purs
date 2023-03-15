module Janus.Page.Home where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Janus.Capability.Navigate (class Navigate)
import Janus.Data.Profile (Profile)
import Janus.Store as Store
import Janus.Component.HTML.Utils (css, prop, safeHref)

import Janus.Data.Username (toString)

data Action
  =  Receive (Connected (Maybe Profile) Unit)
  | Test

type State =
  { currentUser :: Maybe Profile
  }

component
  :: forall q o m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => H.Component q Unit o m
component = connect (selectEq _.currentUser) $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }
  where

    initialState::forall r . {context::Maybe Profile | r} -> State
    initialState { context: currentUser } = { currentUser }

    handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
    handleAction = case _ of
    
      Receive { context: currentUser } -> do
        H.liftEffect $ log $ "Home.Receive " <> show (toString <$> (_.username <$> currentUser))
        H.modify_ _ { currentUser = currentUser }

      Test -> do
        H.liftEffect $ log "Test pressed!"

    render :: forall slots. State -> H.ComponentHTML Action slots m
    render _state@{ currentUser } = 
      HH.button [css "btn btn-lg btn-block btn-warning", HP.type_ HP.ButtonButton, HE.onClick \_-> Test  ] [ HH.text "Test"]

-- |    render _state@{ currentUser } = HH.div [][menu currentUser Home, main $ HH.text "Home"]
