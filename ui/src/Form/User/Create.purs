-- |This module contains the form for creating a new user.
module Janus.Form.User.Create where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Janus.Capability.Resource.User (class ManageUser, createUser)
import Janus.Component.HTML.Utils (css, whenElem)
import Janus.Data.Email (Email)
import Janus.Data.Username (Username)
import Janus.Form.Field as Field
import Janus.Form.Validation (FormError)
import Janus.Form.Validation as V
import Janus.Lang.Form.User (translator, Labels)
import Simple.I18n.Translator (Translator, currentLang, label, setLang, translate)

-- Slot definition for this form
type Slot = forall q. H.Slot q Output Unit

-- The form input
type Input = { country :: String }

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
  , password :: f String FormError String
  )
type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Input Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

-- Inital values of the forms inputs
initialValue :: { username :: String, email :: String, active :: Boolean, password :: String }
initialValue = { username: "", email: "", active: true, password: "" }

-- The actions the form generate
data Action
  = Receive FormContext -- The forms context and input
  | Eval FormlessAction -- The action from the form
  | Cancel -- The for is cancelled

-- The state of the form
type State =
  { form :: FormContext
  , formError :: Boolean
  , i18n :: Translator Labels
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
      }
  } 
  where

    -- The initial state of the component
    initialState context = { form: context, formError: false, i18n: translator context.input.country }

    -- The handler for the forms actions
    handleAction :: Action -> H.HalogenM _ _ _ _ _ Unit
    handleAction = case _ of
      Receive context -> do
        H.modify_ (\state -> state { form = context, i18n = state.i18n # setLang context.input.country })
      Cancel -> do
        F.raise Cancelled
      Eval action -> do
        F.eval action

    -- The handle of the components queries
    handleQuery :: forall a. F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ _ (Maybe a)
    handleQuery = do
      let
        onSubmit o = do
          createUser o
          F.raise Completed

        validation =
          { username: V.required >=> V.minLength 3 >=> V.usernameFormat
          , password: V.required >=> V.minLength 2 >=> V.maxLength 20
          , email: V.required >=> V.minLength 3 >=> V.emailFormat
          , active: Right
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
                        { label: (i18n # translate (label :: _ "email")), state: fields.email, action: actions.email, country: i18n # currentLang }
                        [ HP.type_ HP.InputText ]
                    ]
                ]
            , HH.div [ css "row" ]
                [ HH.div [ css "col" ]
                    [ Field.textInput
                        { label: (i18n # translate (label :: _ "password")), state: fields.password, action: actions.password, country: i18n # currentLang }
                        [ HP.type_ HP.InputPassword ]
                    ],
                    HH.div [ css "col align-self-end" ]
                    [ Field.checkboxInput
                        { label: (i18n # translate (label :: _ "active")), state: fields.active, action: actions.active, country: i18n # currentLang }
                        []
                    ]
                ]
            , Field.submitButton (i18n # translate (label :: _ "create"))
            , HH.span [] [HH.text (" ")]
            , HH.input [ css "btn btn-primary", HP.type_ HP.InputButton, HP.value (i18n # translate (label :: _ "cancel")), HE.onClick \_ -> Cancel ]
            ]
        ]
