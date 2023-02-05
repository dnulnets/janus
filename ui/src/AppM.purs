module Janus.AppM where

import Prelude

import Data.Maybe (fromJust)
import Janus.Api.Endpoint (Endpoint(..))
import Janus.Api.Request (RequestMethod(..))
import Janus.Api.Request as Request
import Janus.Api.Utils (authenticate, decode, mkAuthRequest, mkRequest)
import Janus.Capability.LogMessages (class LogMessages)
import Janus.Capability.Navigate (class Navigate, navigate)
import Janus.Capability.Now (class Now)
import Janus.Capability.Resource.User (class ManageUser)
import Janus.Data.Log as Log
import Janus.Data.Profile as Profile
import Janus.Data.Route as Route
import Janus.Data.Username (parse)
import Janus.Store (Action(..), LogLevel(..), Store)
import Janus.Store as Store
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut as Codec
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Now as Now
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, getStore, runStoreT, updateStore)
import Routing.Duplex (print)
import Routing.Hash (setHash)
import Safe.Coerce (coerce)

newtype AppM a = AppM (StoreT Store.Action Store.Store Aff a)
derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadStoreAppM :: MonadStore Action Store AppM

runAppM ∷ ∀ q i o. Store.Store -> H.Component q i o AppM -> Aff (H.Component q i o Aff)
runAppM store = runStoreT store Store.reduce <<< coerce

instance nowAppM :: Now AppM where
  now = liftEffect Now.now
  nowDate = liftEffect Now.nowDate
  nowTime = liftEffect Now.nowTime
  nowDateTime = liftEffect Now.nowDateTime

instance logMessagesAppM :: LogMessages AppM where
  logMessage log = do
    { logLevel } <- getStore
    liftEffect case logLevel, Log.reason log of
      Prod, Log.Debug -> pure unit
      _, _ -> Console.log $ Log.message log

instance navigateAppM :: Navigate AppM where
  navigate =
    liftEffect <<< setHash <<< print Route.routeCodec

  logout = do
    liftEffect $ Request.removeToken
    updateStore LogoutUser
    navigate Route.Home

instance manageUserAppM :: ManageUser AppM where
  loginUser =
    authenticate Request.login

  getCurrentUser = do
    mbJson <- mkAuthRequest { endpoint: User, method: Get }
    map (map _.user)
      $ decode (CAR.object "User" { user: Profile.profileCodec }) mbJson

  updateUser user = do
    let
      codec = CAR.object "User" { user: Profile.profileWithPasswordCodec }
      method = Put $ Just $ Codec.encode codec { user }

    void $ mkAuthRequest { endpoint: User, method: method }
