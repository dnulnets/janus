-- |This module contains various fields that can be used by the pages or forms for the application.
module Janus.Form.Field
  ( StringField
  , Text
  , MultiSelect
  , checkbox
  , checkboxReadOnly
  , submitButton
  , text
  , textReadOnly
  , multiSelect
  )
  where

import Prelude

import DOM.HTML.Indexed (HTMLinput)
import Data.Array (nub, cons, delete, elemIndex)
import Data.Either (either)
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Janus.Component.HTML.Utils (css, maybeElem)
import Janus.Data.UUID (UUID)
import Janus.Form.Validation (FormError, errorToString)
import Web.Event.Event as Event
import Web.HTML.HTMLSelectElement (HTMLSelectElement)
import Web.HTML.HTMLSelectElement as HTMLSelectElement
import Web.Promise (then_)

type StringField :: (Type -> Type -> Type -> Type) -> Type -> Type
type StringField f output = f String FormError output

submitButton :: forall i p. String -> HH.HTML i p
submitButton label =
  HH.input
    [ css "btn btn-primary"
    , HP.type_ HP.InputSubmit
    , HP.value label
    ]

type Text action output =
  { label :: String
  , state :: F.FieldState String FormError output
  , action :: F.FieldAction action String FormError output
  , locale :: String
  }

text
  :: forall output action slots m
   . Text action output
  -> Array (HP.IProp HTMLinput action)
  -> H.ComponentHTML action slots m
text { label, state, action, locale } props =
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
              HH.text $ errorToString err locale
            ]
      ]
    ]

textReadOnly::forall action slots m . String
  -> String
  -> Array (HP.IProp HTMLinput action)
  -> H.ComponentHTML action slots m
textReadOnly label value props =
  HH.div
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
                , HP.value value
                , HP.readOnly true
                , HP.disabled true
                ]
                props
            )
      ]
    ]

type Checkbox action output =
  { label :: String
  , state :: F.FieldState Boolean Void output
  , action :: F.FieldAction action Boolean Void output
  , locale :: String
  }

checkbox
  :: forall output action slots m
   . Checkbox action output
  -> Array (HP.IProp HTMLinput action)
  -> H.ComponentHTML action slots m
checkbox { label, state, action } props = HH.fieldset []
  [
    HH.div [css "form-check mb-4"] [
      HH.input ( append [css "form-check-input", HE.onChecked action.handleChange, HP.type_ HP.InputCheckbox, HP.id $ "j-" <> label, HP.checked state.value] props),
      HH.label [css "form-check-label", HP.for $ "j-" <> label] [HH.text label]
    ]
  ]

checkboxReadOnly
  :: forall action slots m
   . String
  -> Boolean
  -> Array (HP.IProp HTMLinput action)
  -> H.ComponentHTML action slots m
checkboxReadOnly
 label value props = HH.div []
  [
    HH.div [css "form-check mb-4"] [
      HH.input ( append [css "form-check-input", 
        HP.type_ HP.InputCheckbox, 
        HP.id $ "j-" <> label, 
        HP.checked value,
        HP.readOnly true, HP.disabled true] props),
      HH.label [css "form-check-label", HP.for $ "j-" <> label] [HH.text label]
    ]
  ]

type MultiSelect action input output =
  { label :: String
  , state :: F.FieldState (Array input) Void (Array output)
  , action :: F.FieldAction action (Array input) Void (Array output)
  , options ::
      Array
        { option :: input
        , render :: String
        , props :: Array (HP.IProp HTMLinput action)
        }
  , locale :: String
  }

multiSelect
  :: forall input output action slots m
   . Ord input
  => MultiSelect action input output
  -> H.ComponentHTML action slots m
multiSelect { label, state, action, options } =
  HH.div_
    [ HH.label_ [ HH.text label ]
    , HH.fieldset_ $ options <#> \{ option, render, props } ->
        HH.label_
          [ HH.input $ flip append props
              [ HP.type_ HP.InputCheckbox
              , HP.name action.key
              , HP.checked $ isJust $ elemIndex option state.value
              , HE.onChecked (\b -> handleChecked b option)
              , HE.onBlur action.handleBlur
              ]
          , HH.text render
          ]
    ]

    where

      handleChecked b o = if b then
                          action.handleChange $ nub $ cons o state.value
                        else
                          action.handleChange $ delete o state.value

