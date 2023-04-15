module Janus.Page.Users where

import Janus.Data.UUID
import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Monad (class MonadStore)
import Janus.Capability.Navigate (class Navigate)
import Janus.Capability.Resource.User (class ManageUser, getUsers)
import Janus.Component.Table as Table
import Janus.Store as Store
import Type.Proxy (Proxy(..))

type Input = Unit

data Action =  Receive Input
  | HandleTable Table.Output

type State = {table :: Table.Model}

-- |The componenets that build up the page
type ChildSlots = ( table :: Table.Slot Unit )

component
  :: forall q o m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => ManageUser m
  => H.Component q Input o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }
  where
    initialState _ = {table:{nofItems:0, nofItemsPerPage:5, currentItem:1, action:true, header:[], rows:[]}}
    
    news = {nofItems:2, 
      nofItemsPerPage:5,
      currentItem:1,
      action:true,
      header:["#", "User", "Team"],
      rows:[{key:UUID "5435432-54325432-8765876", row:["1", "tomas", "fragglarna"]}, 
            {key:UUID "6216721-43674367843-31267832", row:["2", "peter", "gurkorna"]}]}

    handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
    handleAction = case _ of  
      Receive i -> do
        H.liftEffect $ log $ "Users.Receive " <> show i
      HandleTable i -> do
        H.liftEffect $ log $ "Users.HandleTable"
        
    render :: State -> H.ComponentHTML Action ChildSlots m
    render s = HH.div [][HH.slot (Proxy :: _ "table") unit Table.component news HandleTable ] 
