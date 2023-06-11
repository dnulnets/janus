-- |This module contains the form for creating a new user.
module Janus.Form.User.Create where

import Janus.Data.Error
import Janus.Data.UUID
import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Janus.Capability.Resource.User (class ManageUser, createUser, updateRoles)
import Janus.Component.HTML.Utils (css, whenElem, prop, maybeElem)
import Janus.Data.Email (Email)
import Janus.Data.Role (RoleType)
import Janus.Data.Role as RT
import Janus.Data.Username (Username)
import Janus.Form.Field as Field
import Janus.Form.Validation (FormError)
import Janus.Form.Validation as V
import Janus.Lang.Form.User (i18n, Phrases)
import Janus.Lang.I18n (I18n, setLocale, message)

-- Slot definition for this form
type Slot = forall q. H.Slot q Output Unit

-- The form input
type Input = { locale :: String }

-- The form output
data Output = 
    Completed -- The form is completed 
  | Cancelled -- The form is cancelled

-- The form
type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( username :: f String FormError Username
  , email :: f String FormError Email
  , active :: f Boolean Void Boolean
  , password :: f String FormError String
  , roles :: f (Array RoleType) Void (Array RoleType)
  )

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Input Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

-- Inital values of the forms inputs
initialValue :: { username :: String, email :: String, active :: Boolean, password :: String, roles :: Array RoleType}
initialValue = { username: "", email: "", active: true, password: "", roles: [] }

-- The actions the form generate
data Action
  = Receive FormContext -- The forms context and input
  | Eval FormlessAction -- The action from the form
  | Cancel -- The for is cancelled

-- The state of the form
type State =
  { form :: FormContext
  , formError :: Boolean
  , i18n :: I18n Phrases
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
      }
  } 
  where

    -- The initial state of the component
    initialState context = { form: context, formError: false, i18n: setLocale i18n context.input.locale, error: Nothing }

    -- The handler for the forms actions
    handleAction :: Action -> H.HalogenM _ _ _ _ _ Unit
    handleAction = case _ of
      Receive context -> do
        H.modify_ (\state -> state { form = context, i18n = setLocale i18n context.input.locale })
      Cancel -> do
        F.raise Cancelled
      Eval action -> do
        F.eval action

    -- The handle of the components queries
    handleQuery :: forall a. F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ _ (Maybe a)
    handleQuery = do
      let
        onSubmit o = do
          r <- createUser {active:o.active, email:o.email, password:o.password, username:o.username}
          H.liftEffect $ log $ show o.roles
          case r of
            Left ae -> do
              i18n <- H.gets _.i18n
              H.modify_ (\s -> s { error = Just (flash i18n ae) })
            Right u -> do
              q <- updateRoles u.key $ map (\rt->{key:Nothing, role:rt}) o.roles
              H.liftEffect $ log $ show q
              F.raise Completed

        validation =
          { username: V.required >=> V.minLength 3 >=> V.usernameFormat
          , password: V.required >=> V.minLength 2 >=> V.maxLength 20
          , email: V.required >=> V.minLength 3 >=> V.emailFormat
          , active: Right
          , roles: Right
          }

      F.handleSubmitValidate onSubmit F.validate validation

    -- Renders the component
    render :: State -> H.ComponentHTML Action () m
    render { i18n: i18n, formError: formError, form: { formActions, fields, actions }, error:error } =
      HH.div [] [
          whenElem (isJust error) \_ -> HH.div [css "alert alert-danger", prop "role" "alert"][maybeElem error HH.text],
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
                            { label: (i18n.dictionary.email), state: fields.email, action: actions.email, locale: i18n.locale }
                            [ HP.type_ HP.InputText ]
                        ]
                    ]
                , HH.div [ css "row" ]
                    [ HH.div [ css "col" ]
                        [ Field.text
                            { label: (i18n.dictionary.password), state: fields.password, action: actions.password, locale: i18n.locale}
                            [ HP.type_ HP.InputPassword ]
                        ],
                        HH.div [ css "col align-self-end" ]
                        [ Field.checkbox
                            { label: (i18n.dictionary.active), state: fields.active, action: actions.active, locale: i18n.locale }
                            []
                        ]
                    ]
                , HH.div [css "row"]
                    [
                      HH.div [css "col"]
                        [
                          Field.multiSelect
                            {label: (i18n.dictionary.roles), state: fields.roles, action: actions.roles, locale: i18n.locale, 
                            options: [
                              {option: RT.User, render: "User", props: []},
                              {option: RT.Administrator, render: "Administrator", props: []},
                              {option: RT.TeamLeader, render: "Team Leader", props: []} ] }
                        ],
                      HH.div [css "col"]
                        [

                        ]
                    ]
                , Field.submitButton (i18n.dictionary.create)
                , HH.span [] [HH.text (" ")]
                , HH.input [ css "btn btn-primary", HP.type_ HP.InputButton, HP.value (i18n.dictionary.cancel), HE.onClick \_ -> Cancel ]
                ]
            ]
      ]