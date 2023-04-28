-- |This module contains the form for creating a new user.
module Janus.Form.User.Delete where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Janus.Capability.Navigate (class Navigate)
import Janus.Capability.Resource.User (class ManageUser, deleteUser, getUser)
import Janus.Component.HTML.Utils (css, whenElem)
import Janus.Data.Email (Email)
import Janus.Data.Profile (Profile(..))
import Janus.Data.UUID (UUID)
import Janus.Data.Username (Username)
import Janus.Form.Field as Field
import Janus.Form.Validation (FormError)
import Janus.Form.Validation as V
import Janus.Lang.Form.User (translator, Labels)
import Simple.I18n.Translator (Translator, currentLang, label, setLang, translate)

-- Slot definition for this form
type Slot = forall q. H.Slot q Output Unit

-- The form input
type Input = {
  country :: String, 
  key :: UUID }

-- The form output
data Output = 
    Completed -- The form is completed 
  | Cancelled -- Theform is cancelled

-- The actions the form generate
data Action
  = Initialize    -- The forms initialization
  | Receive Input -- The forms input
  | Delete        -- The user is deleted
  | Cancel        -- The form is cancelled

-- The state of the form
type State =
  { key :: UUID
  , user :: Maybe Profile
  , i18n :: Translator Labels }

-- The form definiton
component
  :: forall q m
   . MonadAff m
  => Navigate m
  => ManageUser m
  => H.Component q Input Output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { receive = Just <<< Receive
      , handleAction = handleAction
      , initialize = Just Initialize 
      }
  } 
  where

    -- The initial state of the component
    initialState context = { i18n: translator context.country, key: context.key, user:Nothing}

    -- The handler for the forms actions
    handleAction :: Action -> H.HalogenM _ _ _ _ _ Unit
    handleAction = case _ of
      Initialize -> do
        key <- H.gets _.key
        q <- getUser key
        H.modify_ (\state -> state { user = q })
      Receive i -> do
        H.modify_ (\state -> state { i18n = state.i18n # setLang i.country, key = i.key })
        handleAction Initialize
      Delete -> do
        key <- H.gets _.key
        deleteUser key
        H.raise Completed
      Cancel -> do
        H.raise Cancelled

    -- Renders the component
    render :: State -> H.ComponentHTML Action () m
    render { user: Nothing } = HH.div [][]
    render { i18n: i18n, user: Just user } =
      HH.div [ ]
        [ HH.div [ css "row" ]
                [ HH.div [ css "col" ]
                    [ Field.textInputReadOnly
                        (i18n # translate (label :: _ "username"))
                        (show user.username)
                        [ HP.type_ HP.InputText ]
                    ]
                , HH.div [ css "col" ]
                    [ Field.textInputReadOnly
                        (i18n # translate (label :: _ "email"))
                        (show user.email)
                        [ HP.type_ HP.InputText ]
                    ]
                ]
            , HH.input [ css "btn btn-primary", HP.type_ HP.InputButton, HP.value (i18n # translate (label :: _ "delete")), HE.onClick \_ -> Delete ]
            , HH.span [] [HH.text (" ")]
            , HH.input [ css "btn btn-primary", HP.type_ HP.InputButton, HP.value (i18n # translate (label :: _ "cancel")), HE.onClick \_ -> Cancel ]
        ]
