-- |This module contains various functions and utilities for building HTML.
module Janus.Component.HTML.Utils
  ( maybeElem
  , prop
  , safeHref
  , whenElem
  , css)
  where

import Prelude
import Data.Maybe (Maybe(..))

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Routing.Duplex (print)

import Janus.Data.Route (Route, routeCodec)

-- | A class helper function.
css :: forall r i. String -> HH.IProp (class :: String | r) i
css = HP.class_ <<< HH.ClassName

-- | A generic property string function.
prop ∷ forall r i. String
  -> String -> HP.IProp r i
prop name = HP.attr (HH.AttrName name)

-- | a Safe href from Route.
safeHref :: forall r i. Route -> HH.IProp (href :: String | r) i
safeHref = HP.href <<< append "#" <<< print routeCodec

-- | Maybe render a html element.
maybeElem :: forall p i a. Maybe a -> (a -> HH.HTML p i) -> HH.HTML p i
maybeElem (Just x) f = f x
maybeElem _ _ = HH.text ""

-- | Render an element when a condition is true.
whenElem :: forall p i. Boolean -> (Unit -> HH.HTML p i) -> HH.HTML p i
whenElem cond f = if cond then f unit else HH.text ""
