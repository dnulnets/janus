module Janus.Page.Dashboard where

import Prelude

import Data.Lens (Traversal')
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Monoid (guard)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Janus.Capability.Navigate (class Navigate)
import Janus.Component.HTML.Menu (menu)
import Janus.Component.HTML.Utils (css, maybeElem, whenElem)
import Janus.Data.Profile (Profile)
import Janus.Data.Route (Route(..))
import Janus.Store as Store
import Janus.Component.HTML.Fragments (main)
import Network.RemoteData (RemoteData(..), _Success, fromMaybe, toMaybe)
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)
import Janus.Data.Username (toString)

data Action
  =  Receive (Connected (Maybe Profile) Unit)

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
  initialState { context: currentUser } =
    { currentUser
    }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
  
    Receive { context: currentUser } -> do
      H.liftEffect $ log $ "Dashboard.Receive " <> show (toString <$> (_.username <$> currentUser))
      H.modify_ _ { currentUser = currentUser }

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render state@{ currentUser } = HH.div [][menu currentUser Home, main $ HH.text "Dashboard"]
