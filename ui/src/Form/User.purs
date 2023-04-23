-- |This module contains the form for login.
module Janus.Form.User where

import Data.Either
import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (sequence)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Janus.Capability.Navigate (class Navigate, navigate)
import Janus.Capability.Resource.User (class ManageUser, getUser)
import Janus.Component.HTML.Utils (css, whenElem)
import Janus.Data.Email (Email(..))
import Janus.Data.Route (Route(..))
import Janus.Data.UUID (UUID(..))
import Janus.Data.Username (Username(..))
import Janus.Form.Field as Field
import Janus.Form.Validation (FormError)
import Janus.Form.Validation as V
import Janus.Lang.Form.User (translator, Labels)
import Simple.I18n.Translator (Translator, currentLang, label, setLang, translate)

type Slot = forall q . H.Slot q Void Unit

type Input = { country:: String, key::Maybe UUID }

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( username :: f String FormError Username
  , key :: f String FormError (Maybe UUID)
  , email :: f String FormError Email
  , active :: f Boolean Void Boolean
  , password :: f String FormError (Maybe String)
  )

initialValue::{username::String, email::String, active::Boolean, password::String, key::String}
initialValue = {username:"", email:"", active:true, password:"", key:""}

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Input Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

data Action
  = Initialize
  | Receive FormContext
  | Eval FormlessAction

type State =
  { form :: FormContext
  , formError :: Boolean
  , i18n ::  Translator Labels
  }

component
  :: forall query output m
   . MonadAff m
  => Navigate m
  => ManageUser m
  => H.Component query Input output m
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

  initialState context = { form: context, formError: false, i18n: translator context.input.country}

  extract f p = f
          { username { value = show p.username }
          , password { value = "" }
          , email { value = show p.email }
          , key { value = show p.key }
          , active { value = p.active }
          }

  empty f = f
          { username { value = "" }
          , password { value = "" }
          , email { value = "" }
          , key { value = ""}
          , active { value = true }
          }

  handleAction :: Action -> H.HalogenM _ _ _ _ _ Unit
  handleAction = case _ of
    Initialize -> do

      input <- H.gets _.form.input
      q <- sequence $ getUser <$> input.key
      { formActions, fields } <- H.gets _.form
      handleAction $ formActions.setFields $ fromMaybe (empty fields) (extract fields <$> join q)
      H.liftEffect $ log $ "Form.User Initialize"

    Receive context -> do
    
        H.liftEffect $ log $ "Form.User Country = " <> context.input.country
        H.modify_ (\state -> state { form = context, i18n = state.i18n # setLang context.input.country})

    Eval action -> do
        F.eval action

  handleQuery :: forall a. F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = do
    let
      onSubmit _ = do
        H.liftEffect $ log $ "onSubmit"

      validation =
        { username: V.required >=> V.minLength 3 >=> V.usernameFormat
        , password: V.toOptional $ V.required >=> V.minLength 2 >=> V.maxLength 20
        , email: V.required >=> V.minLength 3 >=> V.emailFormat
        , key: V.toOptional V.uuidFormat
        , active: Right
        }

    F.handleSubmitValidate onSubmit F.validate validation

  render :: State -> H.ComponentHTML Action () m
  render { i18n: i18n, formError: formError, form: { formActions, fields, actions } } = 
    HH.form [ HE.onSubmit formActions.handleSubmit ]
    [ whenElem formError \_ ->
        HH.div
            [ css "j-invalid-feedback" ]
            [ HH.text (i18n # translate (label :: _ "invalid")) ]
    , HH.fieldset_
        [ HH.div [css "row"][
            HH.div [css "col"][Field.textInput
              { label: (i18n # translate (label :: _ "username")), state: fields.username, action: actions.username, country: i18n # currentLang }
              [ HP.type_ HP.InputText]]
          , HH.div [css "col"] [
              Field.textInput
                { label: (i18n # translate (label :: _ "key")), state: fields.key, action: actions.key, country: i18n # currentLang }
                [ HP.type_ HP.InputText, HP.disabled true ]
            ]]
        , HH.div [css "row"] [ 
            HH.div [css "col"][
              Field.textInput
                { label: (i18n # translate (label :: _ "email")), state: fields.email, action: actions.email, country: i18n # currentLang }
                [ HP.type_ HP.InputText ]]
            , HH.div [css "col"] [
              Field.textInput
                { label: (i18n # translate (label :: _ "password")), state: fields.password, action: actions.password, country: i18n # currentLang }
                [ HP.type_ HP.InputPassword ]
            ]]
        , HH.div [css "row"] [
            HH.div [css "col"] [
                Field.checkboxInput
                  { label: (i18n # translate (label :: _ "active")), state: fields.active, action: actions.active, country: i18n # currentLang }
                  [ ]]
            ]
        , Field.submitButton "Save"
        ]
    ]
  