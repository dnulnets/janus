-- |This module contains various fields that can be used by the pages or forms for the application.
module Janus.Form.Field
  ( StringField
  , TextInput
  , submitButton
  , textInput
  )
  where

import Prelude

import DOM.HTML.Indexed (HTMLinput)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Janus.Component.HTML.Utils (css, maybeElem)
import Janus.Form.Validation (FormError, errorToString)

type StringField :: (Type -> Type -> Type -> Type) -> Type -> Type
type StringField f output = f String FormError output

submitButton :: forall i p. String -> HH.HTML i p
submitButton label =
  HH.input
    [ css "btn btn-primary"
    , HP.type_ HP.InputSubmit
    , HP.value label
    ]

type TextInput action output =
  { label :: String
  , state :: F.FieldState String FormError output
  , action :: F.FieldAction action String FormError output
  , country :: String
  }

textInput
  :: forall output action slots m
   . TextInput action output
  -> Array (HP.IProp HTMLinput action)
  -> H.ComponentHTML action slots m
textInput { label, state, action, country } props =
  HH.fieldset
    []
    [ HH.div [css "mb-3"]
      [
        HH.label [css "form-label", HP.for $ "j-" <> label]
        [
          HH.text label
        ],
        HH.input
            ( append
                [ css "form-control"
                , HP.id $ "j-" <> label
                , HP.value state.value
                , HE.onValueInput action.handleChange
                , HE.onBlur action.handleBlur
                ]
                props
            )
        , maybeElem (state.result >>= either pure (const Nothing)) \err ->
            HH.div [css "j-invalid-feedback"]
            [
              HH.text $ errorToString err country
            ]
      ]
    ]

