module Janus.Api.Utils
  ( authenticate
  , decode
  , mkAuthRequest
  , mkRequest
  )
  where

import Prelude

import Affjax.Web (request, printError, Response(..), Error)
import Affjax.StatusCode (StatusCode(..))
import Janus.Api.Request (BaseURL, RequestTemplate, Token, defaultRequest, readToken, writeToken)
import Janus.Capability.LogMessages (class LogMessages, logError)
import Janus.Capability.Now (class Now)
import Janus.Data.Profile (Profile)
import Janus.Store (Action(..), Store)
import Data.Argonaut.Core (Json, toString)
import Data.Bifunctor (rmap)
import Data.Codec.Argonaut (JsonCodec, printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)
import Effect.Console (log)

convertBody::Either Error (Response Json) -> Either String Json
convertBody (Left _) = Left $ "\"JAN002\""
convertBody (Right {body:body, status: (StatusCode status)}) = if status>= 200 && status<300 then Right body else Left $ fromMaybe "\"JAN001\"" $ toString body

-- |Performs a request based on the template and returns with the result.
mkRequest
  :: forall m
   . MonadAff m
  => MonadStore Action Store m
  => RequestTemplate
  -> m (Maybe Json)
mkRequest opts = do
  { baseUrl } <- getStore
  response <- liftAff $ request $ defaultRequest baseUrl Nothing opts
  pure $ hush $ rmap _.body response

-- |Performs a request based on the template and use the authroization token. Return with the result.
mkAuthRequest
  :: forall m
   . MonadAff m
  => MonadStore Action Store m
  => RequestTemplate
  -> m (Either String Json)
mkAuthRequest opts = do
  { baseUrl } <- getStore
  token <- liftEffect readToken
  response <- liftAff $ request $ defaultRequest baseUrl token opts
  pure $ convertBody response
  -- pure $ hush $ map _.body response

-- |Authenticate the user and return with the profile and token. It also sets the token in the browser local
-- |storage.
authenticate
  :: forall m a
   . MonadAff m
  => MonadStore Action Store m
  => LogMessages m
  => Now m
  => (BaseURL -> a -> m (Either String (Tuple Token Profile)))
  -> a
  -> m (Maybe Profile)
authenticate req fields = do
  { baseUrl } <- getStore
  req baseUrl fields >>= case _ of
    Left err -> logError err *> pure Nothing
    Right (Tuple token profile) -> do
      liftEffect do
        writeToken token
      updateStore $ LoginUser profile
      pure (Just profile)

-- |Decodes the json to its type and write any erros in the log.
decode :: forall m a. LogMessages m => Now m => JsonCodec a -> Maybe Json -> m (Maybe a)
decode _ Nothing = logError "Response malformed" *> pure Nothing
decode codec (Just json) = case CA.decode codec json of
  Left err -> logError (printJsonDecodeError err) *> pure Nothing
  Right response -> pure (Just response)

