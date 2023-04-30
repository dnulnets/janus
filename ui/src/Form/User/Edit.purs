-- |This module contains the form for creating a new user.
module Janus.Form.User.Edit where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Janus.Capability.Resource.User (class ManageUser, updateUser, getUser)
import Janus.Component.HTML.Utils (css, whenElem)
import Janus.Data.Email (Email)
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
type Input = { country :: String, key :: UUID }

-- The form output
data Output = 
    Completed -- The form is completed 
  | Cancelled -- Theform is cancelled

-- The form
type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( username :: f String FormError Username
  , email :: f String FormError Email
  , active :: f Boolean Void Boolean
  , password :: f String FormError (Maybe String)
  , key :: f String FormError UUID
  )
type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Input Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

-- Inital values of the forms inputs
initialValue :: { username :: String, email :: String, active :: Boolean, password :: String, key :: String }
initialValue = { username: "", email: "", active: true, password: "", key: ""}

-- The actions the form generate
data Action
  = Initialize
  | Receive FormContext -- The forms context and input
  | Eval FormlessAction -- The action from the form
  | Cancel -- The form is cancelled

-- The state of the form
type State =
  { form :: FormContext
  , formError :: Boolean
  , i18n :: Translator Labels
  , key :: UUID
  }

-- The form definiton
component
  :: forall q m
   . MonadAff m
  => ManageUser m
  => H.Component q Input Output m
component = F.formless { liftAction: Eval } initialValue $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { receive = Just <<< Receive
      , handleAction = handleAction
      , handleQuery = handleQuery
      , initialize = Just Initialize
      }
  } 
  where

    -- The initial state of the component
    initialState context = { form: context, formError: false, i18n: translator context.input.country, key: context.input.key }

    extract f p = f
          { username { value = show p.username }
          , password { value = "" }
          , email { value = show p.email }
          , active { value = p.active }
          , key { value = show p.key}
          }

    empty f = f
          { username { value = "" }
          , password { value = "" }
          , email { value = "" }
          , active { value = true }
          , key { value = ""}
          }

    -- The handler for the forms actions
    handleAction :: Action -> H.HalogenM _ _ _ _ _ Unit
    handleAction = case _ of
      Initialize -> do
        key <- H.gets _.form.input.key
        q <- getUser key
        { formActions, fields } <- H.gets _.form
        handleAction $ formActions.setFields $ fromMaybe (empty fields) ((extract fields) <$> q)
      Receive context -> do
        H.modify_ (\state -> state { form = context, i18n = state.i18n # setLang context.input.country, key = context.input.key })
      Cancel -> do
        F.raise Cancelled
      Eval action -> do
        F.eval action

    -- The handle of the components queries
    handleQuery :: forall a. F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ _ (Maybe a)
    handleQuery = do
      let
        onSubmit o = do
          updateUser o
          -- createUser o
          F.raise Completed

        validation =
          { username: V.required >=> V.minLength 3 >=> V.usernameFormat
          , password: V.toOptional $ V.required >=> V.minLength 2 >=> V.maxLength 20
          , email: V.required >=> V.minLength 3 >=> V.emailFormat
          , active: Right
          , key: V.required >=> V.uuidFormat
          }

      F.handleSubmitValidate onSubmit F.validate validation

    -- Renders the component
    render :: State -> H.ComponentHTML Action () m
    render { i18n: i18n, formError: formError, form: { formActions, fields, actions } } =
      HH.form [ HE.onSubmit formActions.handleSubmit ]
        [ whenElem formError \_ ->
            HH.div
              [ css "j-invalid-feedback" ]
              [ HH.text (i18n # translate (label :: _ "invalid")) ]
        , HH.fieldset_
            [ HH.div [ css "row" ]
                [ HH.div [ css "col" ]
                    [ Field.textInput
                        { label: (i18n # translate (label :: _ "username")), state: fields.username, action: actions.username, country: i18n # currentLang }
                        [ HP.type_ HP.InputText ]
                    ]
                , HH.div [ css "col" ]
                    [ Field.textInput
                        { label: (i18n # translate (label :: _ "key")), state: fields.key, action: actions.key, country: i18n # currentLang }
                        [ HP.type_ HP.InputText, HP.disabled true ]
                    ]
                ]
            , HH.div [ css "row" ]
                [ HH.div [ css "col" ]
                    [ Field.textInput
                        { label: (i18n # translate (label :: _ "email")), state: fields.email, action: actions.email, country: i18n # currentLang }
                        [ HP.type_ HP.InputText ]
                    ],
                    HH.div [ css "col" ]
                    [ Field.textInput
                        { label: (i18n # translate (label :: _ "password")), state: fields.password, action: actions.password, country: i18n # currentLang }
                        [ HP.type_ HP.InputPassword ]
                    ]]
            , HH.div [css "row"]
                [ HH.div [ css "col align-self-end" ]
                    [ Field.checkboxInput
                        { label: (i18n # translate (label :: _ "active")), state: fields.active, action: actions.active, country: i18n # currentLang }
                        []
                    ]
                ]
            , Field.submitButton (i18n # translate (label :: _ "save"))
            , HH.span [] [HH.text (" ")]
            , HH.input [ css "btn btn-primary", HP.type_ HP.InputButton, HP.value (i18n # translate (label :: _ "cancel")), HE.onClick \_ -> Cancel ]
            ]
        ]
