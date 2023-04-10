-- |This module contains the component for a table
module Janus.Component.Table where

import Prelude

import Data.Array (range)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Janus.Capability.Navigate (class Navigate)
import Janus.Component.HTML.Utils (css, prop)
import Halogen.HTML.Events as HE

type Slot id = forall q. H.Slot q Output id

type TableRow = Array String
type TableHeader = Array String

type Input = { nofItems :: Int
            , currentItem :: Int
            , nofItemsPerPage :: Int
            , headers :: TableHeader
            , rows :: Array TableRow }

data Output = Page Int
  | Create

data Action = Receive Input
  | GotoPage Int
  | DoCreate

type State = { nofItems :: Int
            , currentItem :: Int
            , nofItemsPerPage :: Int
            , headers :: TableHeader
            , rows :: Array TableRow }

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

  initialState i = i

  handleAction :: Action -> H.HalogenM _ _ _ _ _ Unit
  handleAction = case _ of
    Receive i -> do
      H.liftEffect $ log $ "Table.Receive " <> show i
    GotoPage n -> do
      H.liftEffect $ log $ "Table.Gotopage " <> show n
      H.raise $ Page n     
    DoCreate -> do
      H.liftEffect $ log $ "Table.Create"
      H.raise $ Create


  render :: State -> H.ComponentHTML Action () m
  render {nofItems:nofItems, currentItem:currentItem, nofItemsPerPage:nofItemsPerPage, headers:headers, rows:rows} =
    HH.div [] [ top, table, bottom ]
    where

    top = HH.div [ css "row" ]
      [ HH.div [ css "col d-flex align-items-center justify-content-start" ] [ HH.text $ "Visar " <> show nofItemsPerPage <> " objekt per sida" ],
        HH.div [ css "col d-flex align-items-center justify-content-end" ]
            [ HH.a [ css "btn btn-primary", prop "role" "button", HE.onClick \_ -> DoCreate] [ HH.text "Create" ]]
      ]

    bottom = HH.div [css "row"][
        HH.div [css "col d-flex align-items-start justify-content-start"][
          HH.text $ "Visar objekt " <> show (currentItem + 1) <> " to " <> show (currentItem + min nofItemsPerPage nofItems) <> " of " <> show nofItems,
          HH.br [],
          HH.text $ "Visar sida " <> show (page currentItem nofItemsPerPage) <> " of " <> show (page nofItems nofItemsPerPage)
        ],
        HH.div [css "col d-flex align-items-start justify-content-end"][
          HH.ul [css "pagination"]
            ([ HH.li [css $ "page-item" <> if (page currentItem nofItemsPerPage) == 1 then " disabled" else ""]
                [HH.a [css "page-link", HE.onClick \_ -> GotoPage ((page currentItem nofItemsPerPage)-1)][HH.text "Previous"]]]
            <> (map pageLink (range 1 (page nofItems nofItemsPerPage))) <>
            [ HH.li [css $ "page-item" <> if (page currentItem nofItemsPerPage) == (page nofItems nofItemsPerPage) then " disabled" else ""]
              [HH.a [css "page-link", HE.onClick \_ -> GotoPage ((page currentItem nofItemsPerPage)+1)][HH.text "Next"]]])
        ]
    ]

    header s = HH.th [][HH.text s]

    value s = HH.td [][HH.text s]

    row r = HH.tr [] $ map value r

    page c nip = 1 + c / nip

    pageLink n = HH.li [css $ "page-item"][HH.a [css "page-link", HE.onClick \_->GotoPage n][HH.text $ show n]]

    table = HH.div [ css "row" ] [
        HH.div [ css "col" ] [
            HH.table [css "table table-striped style=\"width:100%\""][
                HH.thead [][HH.tr [] $ map header headers],
                HH.tbody [] $ map row rows
            ]
        ]
    ]
