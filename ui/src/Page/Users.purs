module Janus.Page.Users where

import Prelude

import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Monad (class MonadStore)
import Janus.Capability.Navigate (class Navigate)
import Janus.Capability.Resource.User (class ManageUser, getUsers, nofUsers)
import Janus.Component.HTML.Utils (css)
import Janus.Component.Table as Table
import Janus.Data.Profile (Profile)
import Janus.Data.UUID (UUID(..))
import Janus.Form.User.Create as UserCreate
import Janus.Form.User.Delete as UserDelete
import Janus.Form.User.Edit as UserEdit
import Janus.Lang.Users (Labels, translator)
import Janus.Store as Store
import Simple.I18n.Translator (Translator, currentLang, label, translate)
import Type.Proxy (Proxy(..))
import Web.HTML.Event.EventTypes (offline)

type Input = { country :: String }

data Action
  = Initialize
  | Receive Input
  | HandleTable Table.Output
  | HandleCreate UserCreate.Output
  | HandleDelete UserDelete.Output
  | HandleEdit UserEdit.Output

data View = Table | Change | Remove | Create

type State = { i18n :: Translator Labels, table :: Table.Model, key :: Maybe UUID, view :: View }

-- |The componenets that build up the page
type ChildSlots = (table :: Table.Slot Unit, 
                  user :: UserCreate.Slot,
                  delete :: UserDelete.Slot,
                  edit :: UserEdit.Slot)

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

  initialState i =
    { i18n: translator i.country
    , table:
        { nofItems: 0
        , nofItemsPerPage: 5
        , currentItem: 1
        , action: true
        , header: []
        , rows: []
        }
    , view: Table
    , key: Nothing
    }

  convert :: Profile -> Table.Line
  convert { key: key, email: email, username: username, active: active } = { key: key, row: [ show username, show email, show active, show key ] }

  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      st <- H.get
      ul <- map convert <$> getUsers (st.table.currentItem - 1) st.table.nofItemsPerPage
      n <- nofUsers
      H.modify_
        ( \s -> s
            { table
                { nofItems = n
                , rows = ul
                , header =
                    [ (st.i18n # translate (label :: _ "username"))
                    , (st.i18n # translate (label :: _ "email"))
                    , (st.i18n # translate (label :: _ "active"))
                    , (st.i18n # translate (label :: _ "key"))
                    ]
                }
            }
        )
      H.liftEffect $ log $ "Users.Initialize " <> show n

    Receive i -> do
      H.liftEffect $ log $ "Users.Receive " <> show i
    HandleTable i -> do
      handleTable i
      H.liftEffect $ log $ "Users.HandleTable"
    HandleCreate UserCreate.Cancelled -> do
      H.modify_ (\s -> s { view = Table, key = Nothing })
    HandleDelete UserDelete.Cancelled -> do
      H.modify_ (\s -> s { view = Table, key = Nothing })
    HandleEdit UserEdit.Cancelled -> do
      H.modify_ (\s -> s { view = Table, key = Nothing })
    HandleCreate UserCreate.Completed -> do
      st <- H.get
      ul <- map convert <$> getUsers (st.table.currentItem - 1) st.table.nofItemsPerPage
      nof <- nofUsers
      H.modify_ (\s -> s { view = Table, key = Nothing, table { nofItems = nof, rows = ul } })
    HandleDelete UserDelete.Completed -> do
      st <- H.get
      ul <- map convert <$> getUsers (st.table.currentItem - 1) st.table.nofItemsPerPage
      nof <- nofUsers
      H.modify_ (\s -> s { view = Table, key = Nothing, table { nofItems = nof, rows = ul } })
    HandleEdit UserEdit.Completed -> do
      st <- H.get
      ul <- map convert <$> getUsers (st.table.currentItem - 1) st.table.nofItemsPerPage
      nof <- nofUsers
      H.modify_ (\s -> s { view = Table, key = Nothing, table { nofItems = nof, rows = ul } })


  handleTable :: Table.Output -> H.HalogenM State Action ChildSlots o m Unit
  handleTable = case _ of
    Table.GotoItem n -> do
      st <- H.get
      ul <- map convert <$> getUsers (n - 1) st.table.nofItemsPerPage
      nof <- nofUsers
      H.modify_ (\s -> s { table { currentItem = n, nofItems = nof, rows = ul } })
      H.liftEffect $ log $ "User.GotoItem" <> show n
    Table.Create -> do
      H.modify_ (\s -> s { view = Create })
      H.liftEffect $ log $ "User.Create"
    Table.Delete u -> do
      H.modify_ (\s -> s { view = Remove, key = Just u })
      H.liftEffect $ log $ "User.Delete " <> show u
    Table.Edit u -> do
      H.modify_ (\s -> s { view = Change, key = Just u })
      H.liftEffect $ log $ "User.Edit " <> show u

  render :: State -> H.ComponentHTML Action ChildSlots m
  render s = HH.div [ css "container mt-3" ]
    [ HH.h1 [] [ HH.text (s.i18n # translate (label :: _ "title")) ]
    , case s.view of
        Table -> HH.slot (Proxy :: _ "table") unit Table.component { country: s.i18n # currentLang, model: s.table } HandleTable
        Change -> case s.key of
          (Just key) -> HH.slot (Proxy :: _ "edit") unit UserEdit.component { country: s.i18n # currentLang, key: key} HandleEdit
          Nothing -> HH.div [][]
        Remove -> case s.key of
          (Just key) -> HH.slot (Proxy :: _ "delete") unit UserDelete.component { country: s.i18n # currentLang, key: key} HandleDelete
          Nothing -> HH.div [][]
        Create -> HH.slot (Proxy :: _ "user") unit UserCreate.component { country: s.i18n # currentLang } HandleCreate
    ]
