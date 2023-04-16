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
import Halogen.Store.Select (selectEq, selectAll)
import Janus.Capability.Navigate (class Navigate)
import Janus.Capability.Resource.User (class ManageUser, getCurrentUser, getUsers)
import Janus.Data.Profile (Profile)
import Janus.Store as Store
import Janus.Component.HTML.Utils (css, prop, safeHref)

data Input = Unit

data Action
  =  Receive (Connected Store.Store Input)
  | Test

type State =
  { currentUser :: Maybe Profile
  }

component
  :: forall q o m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => ManageUser m
  => H.Component q Input o m
component = connect selectAll $ H.mkComponent
  { initialState: deriveState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }
  where

    deriveState :: Connected Store.Store Input -> State
    deriveState { context, input } = { currentUser: context.currentUser }

    handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
    handleAction = case _ of
    
      Receive { context } -> do
        H.liftEffect $ log $ "Home.Receive " <> show (show <$> (_.username <$> context.currentUser))
        H.modify_ _ { currentUser = context.currentUser }

      Test -> do
        H.liftEffect $ log "Test pressed!"
        user <- getUsers 0 5
        H.liftEffect $ log $ show user

    render :: forall slots. State -> H.ComponentHTML Action slots m
    render _state@{ currentUser } = 
      HH.button [css "btn btn-lg btn-block btn-warning", HP.type_ HP.ButtonButton, HE.onClick \_-> Test  ] [ HH.text "Test"]

-- |    render _state@{ currentUser } = HH.div [][menu currentUser Home, main $ HH.text "Home"]
