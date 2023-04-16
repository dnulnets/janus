-- |This module contains the component for a table. The table item and page index starts at one.
module Janus.Component.Table
  ( Header
  , Input
  , Line
  , Model
  , Output(..)
  , Slot
  , component
  ) where

import Janus.Data.UUID (UUID)
import Prelude

import Data.Array (range)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Janus.Capability.Navigate (class Navigate)
import Janus.Component.HTML.Utils (css, prop)
import Janus.Lang.Component.Table
import Simple.I18n.Translator (Translator, currentLang, label, setLang, translate)

type Slot id = forall q. H.Slot q Output id

type Line = { row :: Array String, key :: UUID }

type Header = Array String

type Model =
  { nofItems :: Int
  , nofItemsPerPage :: Int
  , currentItem :: Int
  , action :: Boolean
  , header :: Header
  , rows :: Array Line
  }

type Input = {
  model::Model
  , country::String }

type State = {
  model::Model
  , i18n :: Translator Labels }

data Output
  = GotoItem Int
  | Create
  | Edit UUID
  | Delete UUID

data Action
  = Receive Input
  | DoGotoItem Int
  | DoCreate
  | DoEdit UUID
  | DoDelete UUID

component
  :: forall query m
   . MonadAff m
  => Navigate m
  => H.Component query Input Output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { receive = Just <<< Receive
      , handleAction = handleAction
      }
  }
  where

  initialState i = { i18n:translator i.country, model:i.model }

  handleAction :: forall slots. Action -> H.HalogenM State Action slots Output m Unit
  handleAction = case _ of

    Receive i -> do
      H.liftEffect $ log $ "Table.Receive "
      H.put { i18n:translator i.country, model:i.model }
    DoGotoItem n -> do
      H.liftEffect $ log $ "Table.Gotopage " <> show n
      H.raise $ GotoItem n
    DoCreate -> do
      H.liftEffect $ log $ "Table.Create"
      H.raise $ Create
    DoDelete id -> do
      H.liftEffect $ log $ "Table.Delete " <> show id
      H.raise $ Delete id
    DoEdit id -> do
      H.liftEffect $ log $ "Table.Edit " <> show id
      H.raise $ Edit id

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { i18n:i18n, model: {action: action, nofItems: nofItems, currentItem: currentItem, nofItemsPerPage: nofItemsPerPage, 
    header: header, rows: rows }} =
    HH.div [] [ top, table, bottom ]
    where

    top = HH.div [ css "row" ]
      [ HH.div [ css "col d-flex align-items-center justify-content-start" ] [ HH.text $ (i18n # translate (label :: _ "shows")) <> 
        show nofItemsPerPage <> (i18n # translate (label :: _ "objects")) ]
      , HH.div [ css "col d-flex align-items-center justify-content-end" ]
          [ HH.a [ css "btn btn-primary btn-sm", prop "role" "button", HE.onClick \_ -> DoCreate ] [ HH.text (i18n # translate (label :: _ "create")) ] ]
      ]

    bottom = HH.div [ css "row" ]
      [ HH.div [ css "col d-flex align-items-start justify-content-start" ]
          [ HH.text $ (i18n # translate (label :: _ "showobject")) <> show currentItem <> (i18n # translate (label :: _ "to")) <> 
              show (currentItem - 1 + min nofItemsPerPage nofItems) <> (i18n # translate (label :: _ "of")) <> show nofItems
          , HH.br []
          , HH.text $ (i18n # translate (label :: _ "showpage")) <> show (page currentItem nofItemsPerPage) <> 
              (i18n # translate (label :: _ "of")) <> show (page (nofItems-1) nofItemsPerPage)
          ]
      , HH.div [ css "col d-flex align-items-start justify-content-end" ]
          [ HH.ul [ css "pagination pagination-sm" ]
              ( [ HH.li [ css $ "page-item" <> if (page currentItem nofItemsPerPage) == 1 then " disabled" else "" ]
                    [ HH.a [ css "page-link", HE.onClick \_ -> DoGotoItem (currentItem - nofItemsPerPage) ] [ HH.text (i18n # translate (label :: _ "previous")) ] ]
                ]
                  <> (map pageLink (range 1 (page (nofItems-1) nofItemsPerPage)))
                  <>
                    [ HH.li [ css $ "page-item" <> if (page currentItem nofItemsPerPage) == (page nofItems nofItemsPerPage) then " disabled" else "" ]
                        [ HH.a [ css "page-link", HE.onClick \_ -> DoGotoItem (currentItem + nofItemsPerPage) ] [ HH.text (i18n # translate (label :: _ "next")) ] ]
                    ]
              )
          ]
      ]

    head s = HH.th [] [ HH.text s ]

    value s = HH.td [] [ HH.text s ]

    row true r = HH.tr [] $ (map value r.row) <>
      [ HH.td [ prop "style" "text-align:right" ]
          [ HH.a [ css "btn btn-primary btn-sm", HE.onClick \_ -> DoEdit r.key ]
              [ HH.text (i18n # translate (label :: _ "edit")) ]
          , HH.span [] [ HH.text " " ]
          , HH.a [ css "btn btn-primary btn-sm", HE.onClick \_ -> DoDelete r.key ]
              [ HH.text (i18n # translate (label :: _ "delete")) ]
          ]
      ]
    row false r = HH.tr [] $ map value r.row

    page c nip = 1 + c / nip

    pageLink n = HH.li [ css $ "page-item" ] [ HH.a [ css "page-link", HE.onClick \_ -> DoGotoItem ((n-1)*nofItemsPerPage+1) ] [ HH.text $ show n] ]

    table = HH.div [ css "row" ]
      [ HH.div [ css "col" ]
          [ HH.table [ css "table table-striped style=\"width:100%\"" ]
              [ HH.thead [] [ HH.tr [] $ (map head header) <> (if action then [ HH.th [ prop "style" "text-align:right" ] [ HH.text "Action" ] ] else []) ]
              , HH.tbody [] $ map (row action) rows
              ]
          ]
      ]
