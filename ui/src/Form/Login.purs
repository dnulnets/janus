-- |This module contains the form for login.
module Janus.Form.Login where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Janus.Capability.Navigate (class Navigate, navigate)
import Janus.Capability.Resource.User (class ManageUser, loginUser)
import Janus.Component.HTML.Utils (css, whenElem)
import Janus.Data.Route (Route(..))
import Janus.Data.Username (Username)
import Janus.Form.Field as Field
import Janus.Form.Validation (FormError)
import Janus.Form.Validation as V
import Janus.Lang.Form.Login (i18n, Phrases)
import Janus.Lang.I18n (I18n, setLocale)

type Slot = forall q . H.Slot q Void Unit

type Input = { redirect:: Boolean, locale:: String}

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( username :: f String FormError Username
  , password :: f String FormError String
  )

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Input Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

data Action
  = Receive FormContext
  | Eval FormlessAction

type State =
  { form :: FormContext
  , loginError :: Boolean
  , i18n :: I18n Phrases
  }

component
  :: forall query output m
   . MonadAff m
  => Navigate m
  => ManageUser m
  => H.Component query Input output m
component = F.formless { liftAction: Eval } mempty $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { receive = Just <<< Receive
      , handleAction = handleAction
      , handleQuery = handleQuery
      }
  }
  where

  initialState context = { form: context, loginError: false, i18n: setLocale i18n context.input.locale }

  handleAction :: Action -> H.HalogenM _ _ _ _ _ Unit
  handleAction = case _ of
    Receive context -> do
        H.liftEffect $ log $ "Form.Login Locale = " <> context.input.locale
        H.modify_ (\state -> state { form = context, i18n = setLocale state.i18n context.input.locale})
    Eval action -> do
        F.eval action

  handleQuery :: forall a. F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = do
    let
      onSubmit = loginUser >=> case _ of
        Nothing -> do
          H.modify_ _ { loginError = true }
        Just _ -> do
          H.modify_ _ { loginError = false }
          navigate Home
          { redirect } <- H.gets _.form.input
          when redirect (navigate Home)

      validation =
        { username: V.required >=> V.minLength 3 >=> V.usernameFormat
        , password: V.required >=> V.minLength 2 >=> V.maxLength 20
        }

    F.handleSubmitValidate onSubmit F.validate validation

  render :: State -> H.ComponentHTML Action () m
  render { i18n: i18n, loginError: loginError, form: { formActions, fields, actions } } = 
    HH.form [ HE.onSubmit formActions.handleSubmit ]
    [ whenElem loginError \_ ->
        HH.div
            [ css "j-invalid-feedback" ]
            [ HH.text i18n.dictionary.invalid ]
    , HH.fieldset_
        [ Field.text
            { label: i18n.dictionary.uname, state: fields.username, action: actions.username, locale: i18n.locale }
            [ HP.type_ HP.InputText ]
        , Field.text
            { label: i18n.dictionary.pwd, state: fields.password, action: actions.password, locale: i18n.locale }
            [ HP.type_ HP.InputPassword ]
        , Field.submitButton i18n.dictionary.login
        ]
    ]