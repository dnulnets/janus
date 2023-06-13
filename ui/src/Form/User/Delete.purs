-- |This module contains the form for creating a new user.
module Janus.Form.User.Delete where

import Prelude

import Data.Maybe (Maybe(..), isJust)
import Data.Either (hush, blush)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Janus.Capability.Navigate (class Navigate)
import Janus.Capability.Resource.User (class ManageUser, deleteUser, getRoles, getUser)
import Janus.Component.HTML.Utils (css, prop, whenElem, maybeElem)
import Janus.Data.Error (flash)
import Janus.Data.Profile (Profile)
import Janus.Data.Role (Role)
import Janus.Data.UUID (UUID)
import Janus.Form.Field as Field
import Janus.Lang.Form.User (i18n, Phrases)
import Janus.Lang.I18n (I18n, setLocale)

-- Slot definition for this form
type Slot = forall q. H.Slot q Output Unit

-- The form input
type Input = {
  locale :: String, 
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
  , roles :: Array Role
  , i18n :: I18n Phrases
  , error :: Maybe String}

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
    initialState context = { i18n: setLocale i18n context.locale, key: context.key, user:Nothing, roles:[], error:Nothing}

    -- The handler for the forms actions
    handleAction :: Action -> H.HalogenM _ _ _ _ _ Unit
    handleAction = case _ of
      Initialize -> do
        key <- H.gets _.key
        q <- getUser key
        r <- getRoles key
        H.modify_ (\state -> state { user = hush q, roles = r, error = flash i18n <$> (blush q) })
      Receive i -> do
        H.modify_ (\state -> state { i18n = setLocale i18n i.locale, key = i.key })
        handleAction Initialize
      Delete -> do
        key <- H.gets _.key
        err <- deleteUser key
        case err of
          Just ae -> do
            i18n <- H.gets _.i18n
            H.modify_ (\s -> s { error = Just (flash i18n ae) })
          Nothing -> do
            H.raise Completed
      Cancel -> do
        H.raise Cancelled

    -- Renders the component
    render :: State -> H.ComponentHTML Action () m
    render { user: Nothing } = HH.div [][]
    render { i18n: i18n, user: Just user, error: error } =
      HH.div [ ]
        [ whenElem (isJust error) \_ -> HH.div [css "alert alert-danger", prop "role" "alert"][maybeElem error HH.text]
        , HH.div [ css "row" ]
                [ HH.div [ css "col" ]
                    [ Field.textReadOnly
                        (i18n.dictionary.username)
                        (show user.username)
                        [ HP.type_ HP.InputText]
                    ]
                , HH.div [ css "col" ]
                    [ Field.textReadOnly
                        (i18n.dictionary.email)
                        (show user.email)
                        [ HP.type_ HP.InputText]
                    ]
                ]
          , HH.div [css "ro"][
              HH.div [css "col"][
                
              ]
            ]
            , HH.input [ css "btn btn-primary", HP.type_ HP.InputButton, HP.value (i18n.dictionary.delete), HE.onClick \_ -> Delete ]
            , HH.span [] [HH.text (" ")]
            , HH.input [ css "btn btn-primary", HP.type_ HP.InputButton, HP.value (i18n.dictionary.cancel), HE.onClick \_ -> Cancel ]
        ]
