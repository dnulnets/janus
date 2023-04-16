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
import Janus.Capability.Resource.User (class ManageUser, getUsers, nofUsers)
import Janus.Component.Table (Output(..))
import Janus.Component.Table as Table
import Janus.Data.Profile
import Janus.Data.Username
import Janus.Data.Email
import Janus.Store as Store
import Type.Proxy (Proxy(..))
import Janus.Lang.Users
import Simple.I18n.Translator (Translator, currentLang, label, setLang, translate)
import Janus.Component.HTML.Utils (css, prop)

type Input = {country::String}

data Action =  Initialize
  | Receive Input
  | HandleTable Table.Output

type State = {i18n::Translator Labels, table :: Table.Model}

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
      , initialize = Just Initialize
      }
  }
  where

    initialState i = {i18n:translator i.country, table:{nofItems:0, nofItemsPerPage:5, currentItem:1, action:true,
        header:[], rows:[]}}

    convert::Profile->Table.Line
    convert {guid:guid, email:email, username:username, active:active} = {key:guid, row:[show username, show email, show active, show guid]}

    handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
    handleAction = case _ of  
      Initialize -> do
        st <- H.get
        ul <- map convert <$> getUsers (st.table.currentItem-1) st.table.nofItemsPerPage 
        n <- nofUsers
        H.modify_ (\s->s {table { nofItems = n, rows = ul, header = [(st.i18n # translate (label :: _ "username")), 
            (st.i18n # translate (label :: _ "email")), 
            (st.i18n # translate (label :: _ "active")),
            (st.i18n # translate (label :: _ "guid"))]}})
        H.liftEffect $ log $ "Users.Initialize " <> show n

      Receive i -> do
        H.liftEffect $ log $ "Users.Receive " <> show i
      HandleTable i -> do
        handleTable i
        H.liftEffect $ log $ "Users.HandleTable"
    
    handleTable:: Table.Output -> H.HalogenM State Action ChildSlots o m Unit
    handleTable = case _ of
      GotoItem n -> do
        st <- H.get
        ul <- map convert <$> getUsers (n-1) st.table.nofItemsPerPage 
        nof <- nofUsers
        H.modify_ (\s->s {table { currentItem = n, nofItems = nof, rows = ul}})
        H.liftEffect $ log $ "User.GotoItem" <> show n
      Create -> do
        H.liftEffect $ log $ "User.Create"
      Delete u -> do
        H.liftEffect $ log $ "User.Delete" <> show u
      Edit u -> do
        H.liftEffect $ log $ "User.Edit" <> show u

    render :: State -> H.ComponentHTML Action ChildSlots m
    render s = HH.div [css "container mt-3"][HH.h1 [][HH.text (s.i18n # translate (label :: _ "title"))], 
        HH.slot (Proxy :: _ "table") unit Table.component {country: s.i18n # currentLang, model: s.table} HandleTable ] 
