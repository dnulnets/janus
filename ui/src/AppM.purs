-- | The application module that defines the application datatype and the actual implementation of
-- | the capabilities for the application.
module Janus.AppM where

import Prelude

import Data.Codec.Argonaut as Codec
import Data.Codec.Argonaut.Record as CAR
import Data.Either (hush, Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (sequence)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Console as Console
import Effect.Now as Now
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, getStore, runStoreT, updateStore)
import Janus.Api.Endpoint (Endpoint(..))
import Janus.Api.Request (RequestMethod(..))
import Janus.Api.Request as Request
import Janus.Api.Utils (authenticate, decode, mkAuthRequest)
import Janus.Capability.LogMessages (class LogMessages)
import Janus.Capability.Navigate (class Navigate, navigate)
import Janus.Capability.Now (class Now)
import Janus.Capability.Resource.User (class ManageUser)
import Janus.Data.Log as Log
import Janus.Data.Profile as Profile
import Janus.Data.Role as Role
import Janus.Data.Route as Route
import Janus.Store (Action(..), LogLevel(..), Store)
import Janus.Store as Store
import Routing.Duplex (print)
import Routing.Hash (setHash)
import Safe.Coerce (coerce)
import Web.HTML.Event.EventTypes (offline)

-- | The definition of the application.
newtype AppM a = AppM (StoreT Store.Action Store.Store Aff a)

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadStoreAppM :: MonadStore Action Store AppM

-- | Creates a component that halogen can run.
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
    mbJson <- mkAuthRequest { endpoint: Login, method: Get }
    case mbJson of
      Left e -> pure Nothing
      Right j -> do
        d <- decode (CAR.object "User" { user: Profile.profileCodec }) j
        case d of
          Left e -> pure Nothing
          Right r -> pure $ Just $ r.user


  updateUser user = do
    let
      codec = CAR.object "User" { user: Profile.profileWithPasswordCodec }
      method = Post $ Just $ Codec.encode codec { user }
    r <- mkAuthRequest { endpoint: User user.key, method: method }
    case r of
      Left e -> pure $ Just e
      Right _ -> pure Nothing

  getUser uuid = do
    mbJson <- mkAuthRequest { endpoint: User uuid, method: Get }
    d <- join <$> (sequence $ (decode (CAR.object "User" { user: Profile.profileCodec })) <$> mbJson)
    pure $ (_.user) <$> d

  deleteUser uuid = do
    r <- mkAuthRequest { endpoint: User uuid, method: Delete }
    case r of
      Left e -> pure $ Just e
      Right _ -> pure Nothing
 
  createUser user = do
    let
      codec = CAR.object "User" { user: Profile.newProfileCodec }
      method = Put $ Just $ Codec.encode codec { user }
    mbJson <- mkAuthRequest { endpoint: CreateUser, method: method }
    d <- join <$> (sequence $ (decode (CAR.object "User" { user: Profile.profileCodec })) <$> mbJson)
    pure $ (_.user) <$> d

  getUsers o n = do
    let
      codec =  Codec.array (CAR.object "User" { user: Profile.profileCodec })
    mbJson <- mkAuthRequest { endpoint: Users {offset:o, n:n}, method: Get }
    case mbJson of
      Left e -> pure []
      Right j -> do
        d <- decode codec j
        case d of
          Left e -> pure []
          Right r -> pure $ map (_.user) r

  nofUsers = do
    let codec = Codec.int
    mbJson <- mkAuthRequest { endpoint: NofUsers, method: Get }
    case mbJson of
      Left e -> pure 0
      Right j -> do
        l <- decode codec j
        case l of
          Left e -> pure 0
          Right r -> pure r
          
  getRoles u = do
    let
      codec =  Codec.array (CAR.object "Role" { role: Role.roleCodec })
    mbJson <- mkAuthRequest { endpoint: Role u, method: Get }
    case mbJson of
      Left e -> pure []
      Right j -> do
        d <- decode codec j
        case d of
          Left e -> pure []
          Right r -> pure $ map (_.role) r

  updateRoles u ar = do
    let
      codec =  Codec.array (CAR.object "Role" { role: Role.roleCodec })
      method = Post $ Just $ Codec.encode codec (map (\r-> {role: r}) ar)
    r <- mkAuthRequest { endpoint: Role u, method: method }
    case r of
      Left e -> pure $ Just e
      Right _ -> pure Nothing
