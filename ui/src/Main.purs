-- | The main module for the application. It sets up the configuration, adds support for
-- | retrieving the user if we already have a valid token in our localstore,
-- | sets the initial state of the global store, handles URL-changes and runs the application.
module Main (main) where

import Prelude

import Affjax.Web (printError, request)
import Janus.Api.Endpoint (Endpoint(..))
import Janus.Api.Request (BaseURL(..), RequestMethod(..), defaultRequest, readToken, readCountry)
import Janus.AppM (runAppM)
import Janus.Component.Router as Router
import Janus.Data.Profile (Profile)
import Janus.Data.Profile as Profile
import Janus.Data.Route (routeCodec)
import Janus.Store (LogLevel(..), Store)
import Data.Bifunctor (lmap)
import Data.Codec as Codec
import Data.Codec.Argonaut (printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)

-- | The main function for the application.
main ∷ Effect Unit
main = HA.runHalogenAff do

  body ← HA.awaitBody

  let
    baseUrl = BaseURL "http://localhost:8080" -- Has to get the origin, will fixa later!
    logLevel = Dev

  country <- liftEffect readCountry
  currentUser :: Maybe Profile ← (liftEffect readToken) >>= case _ of
    Nothing →
      pure Nothing

    Just token → do
      let requestOptions = { endpoint: User, method: Get }
      res ← request $ defaultRequest baseUrl (Just token) requestOptions

      let
        user ∷ Either String Profile
        user = case res of
          Left e →
            Left (printError e)
          Right v → lmap printJsonDecodeError do
            u ← Codec.decode (CAR.object "User" { user: CA.json }) v.body
            CA.decode Profile.profileCodec u.user

      pure $ hush user

  let
    initialStore ∷ Store
    initialStore = { baseUrl: baseUrl, logLevel: logLevel, currentUser: currentUser, country: fromMaybe "en" country }

  rootComponent ← runAppM initialStore Router.component

  halogenIO ← runUI rootComponent unit body

  void $ liftEffect $ matchesWith (parse routeCodec) \old new ->
    when (old /= Just new) $ launchAff_ do
      _response ← halogenIO.query $ H.mkTell $ Router.Navigate new
      pure unit
