module Janus.Component.Utils where

import Prelude

import Halogen as H

-- |Helper for defining an Opaque component, such as the main pages for the router component.
type OpaqueSlot slot = forall query. H.Slot query Void slot
