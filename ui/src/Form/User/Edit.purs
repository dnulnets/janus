-- |This module contains the form for creating a new user.
module Janus.Form.User.Edit where

import Janus.Data.Error
import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Janus.Capability.Resource.User (class ManageUser, updateUser, getUser, getRoles, updateRoles)
import Janus.Component.HTML.Utils (css, whenElem, prop, maybeElem)
import Janus.Data.Email (Email)
import Janus.Data.Role (RoleType)
import Janus.Data.Role as RT
import Janus.Data.UUID (UUID)
import Janus.Data.Username (Username)
import Janus.Form.Field as Field
import Janus.Form.Validation (FormError)
import Janus.Form.Validation as V
import Janus.Lang.Form.User (i18n, Phrases)
import Janus.Lang.I18n (I18n, setLocale, message)

-- Slot definition for this form
type Slot = forall q. H.Slot q Output Unit

-- The form input
type Input = { locale :: String, key :: UUID }

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
  , roles :: f (Array RoleType) Void (Array RoleType)
  )
type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Input Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

-- Inital values of the forms inputs
initialValue :: { username :: String, email :: String, active :: Boolean, password :: String, key :: String, roles :: Array RoleType }
initialValue = { username: "", email: "", active: true, password: "", key: "", roles: []}

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
  , i18n :: I18n Phrases
  , key :: UUID
  , error :: Maybe String
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
    initialState context = { error:Nothing, form: context, formError: false, i18n: setLocale i18n context.input.locale, key: context.input.key }

    extract f r p = f
          { username { value = show p.username }
          , password { value = "" }
          , email { value = show p.email }
          , active { value = p.active }
          , key { value = show p.key}
          , roles { value = r }
          }

    empty f = f
          { username { value = "" }
          , password { value = "" }
          , email { value = "" }
          , active { value = true }
          , key { value = ""}
          , roles { value = [] }
          }

    -- The handler for the forms actions
    handleAction :: Action -> H.HalogenM _ _ _ _ _ Unit
    handleAction = case _ of
      Initialize -> do
        key <- H.gets _.form.input.key
        q <- getUser key
        r <- getRoles key
        { formActions, fields } <- H.gets _.form
        handleAction $ formActions.setFields $ fromMaybe (empty fields) ((extract fields (map (_.role) r)) <$> q)
      Receive context -> do
        H.modify_ (\state -> state { form = context, i18n = setLocale i18n context.input.locale, key = context.input.key })
      Cancel -> do
        key <- H.gets _.form.input.key
        r <- getRoles key
        H.liftEffect $ log $ show r
        F.raise Cancelled
      Eval action -> do
        F.eval action

    -- The handle of the components queries
    handleQuery :: forall a. F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ _ (Maybe a)
    handleQuery = do
      let
        onSubmit o = do
          err <- updateUser {active:o.active, email:o.email, password:o.password, username:o.username, key:o.key}
          case err of
            Just ae -> do
              i18n <- H.gets _.i18n
              H.modify_ (\s -> s { error = Just (flash i18n ae) })
            Nothing -> do
              rerr <- updateRoles o.key $ map (\rt->{key:Nothing, role:rt}) o.roles
              case rerr of
                Just ae -> do
                  i18n <- H.gets _.i18n
                  H.modify_ (\s -> s { error = Just (flash i18n ae) })
                Nothing -> do
                  F.raise Completed

        validation =
          { username: V.required >=> V.minLength 3 >=> V.usernameFormat
          , password: V.toOptional $ V.required >=> V.minLength 2 >=> V.maxLength 20
          , email: V.required >=> V.minLength 3 >=> V.emailFormat
          , active: Right
          , key: V.required >=> V.uuidFormat
          , roles: Right
          }

      F.handleSubmitValidate onSubmit F.validate validation

    -- Renders the component
    render :: State -> H.ComponentHTML Action () m
    render { error:error, i18n: i18n, formError: formError, form: { formActions, fields, actions }} =
      HH.div [] [
        whenElem (isJust error) \_ -> HH.div [css "alert alert-danger", prop "role" "alert"][ maybeElem error HH.text],
        HH.form [ HE.onSubmit formActions.handleSubmit ]
          [ whenElem formError \_ ->
              HH.div
                [ css "j-invalid-feedback" ]
                [ HH.text (i18n.dictionary.invalid) ]
          , HH.fieldset_
              [ HH.div [ css "row" ]
                  [ HH.div [ css "col" ]
                      [ Field.text
                          { label: (i18n.dictionary.username), state: fields.username, action: actions.username, locale: i18n.locale }
                          [ HP.type_ HP.InputText ]
                      ]
                  , HH.div [ css "col" ]
                      [ Field.text
                          { label: (i18n.dictionary.key), state: fields.key, action: actions.key, locale: i18n.locale }
                          [ HP.type_ HP.InputText, HP.disabled true ]
                      ]
                  ]
              , HH.div [ css "row" ]
                  [ HH.div [ css "col" ]
                      [ Field.text
                          { label: (i18n.dictionary.email), state: fields.email, action: actions.email, locale: i18n.locale }
                          [ HP.type_ HP.InputText ]
                      ],
                      HH.div [ css "col" ]
                      [ Field.text
                          { label: (i18n.dictionary.password), state: fields.password, action: actions.password, locale: i18n.locale }
                          [ HP.type_ HP.InputPassword ]
                      ]]
              , HH.div [css "row"]
                  [ HH.div [ css "col align-self-start" ]
                      [ Field.checkbox
                          { label: (i18n.dictionary.active), state: fields.active, action: actions.active, locale: i18n.locale }
                          []
                      ],
                    HH.div [css "col"]
                      [
                        Field.multiSelect
                          {label: (i18n.dictionary.roles), state: fields.roles, action: actions.roles, locale: i18n.locale, 
                          options: [
                            {option: RT.User, render: show RT.User, props: []},
                            {option: RT.Administrator, render: show RT.Administrator, props: []},
                            {option: RT.TeamLeader, render: show RT.TeamLeader, props: []} ] }
                      ]                      
                  ]
              , Field.submitButton (i18n.dictionary.save)
              , HH.span [] [HH.text (" ")]
              , HH.input [ css "btn btn-primary", HP.type_ HP.InputButton, HP.value (i18n.dictionary.cancel), HE.onClick \_ -> Cancel ]
              ]
          ]
        ]