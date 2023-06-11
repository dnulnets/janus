module Janus.Api.Utils
  ( authenticate
  , decode
  , mkAuthRequest
  , mkRequest
  )
  where

import Prelude

import Affjax.StatusCode (StatusCode(..))
import Affjax.Web (Error, Response(..), printError, request)
import Data.Argonaut.Core (Json, toString)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (rmap)
import Data.Codec.Argonaut (JsonCodec, printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)
import Janus.Api.Request (BaseURL, RequestTemplate, Token, defaultRequest, readToken, writeToken)
import Janus.Capability.LogMessages (class LogMessages, logError)
import Janus.Capability.Now (class Now)
import Janus.Data.Error as E
import Janus.Data.Profile (Profile)
import Janus.Store (Action(..), Store)

convertBody::Either Error (Response Json) -> Either E.Error Json
convertBody (Left e) = Left { code:"JAN002", status:Nothing, extra: Just (printError e)}
convertBody (Right {body:body, status: (StatusCode status)}) = if status>= 200 && status<300 then Right body else 
  case decode_ E.errorCodec body of
    Left e -> Left {code:"JAN002", status:Just status, extra: Just e}
    Right b -> Left $ b {status = Just status}

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
  -> m (Either E.Error Json)
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
decode :: forall m a. LogMessages m => Now m => JsonCodec a -> Json -> m (Either E.Error a)
decode codec json = case CA.decode codec json of
  Left err -> logError (printJsonDecodeError err) *> (pure $ Left {code:"JAN005", status: Nothing, extra: Just $ printJsonDecodeError err})
  Right response -> pure $ Right response

-- |Decodes the json to its type and write any erros in the log.
decode_ :: forall a. JsonCodec a -> Json -> Either String a
decode_ codec json = case CA.decode codec json of
  Left err -> Left $ printJsonDecodeError err
  Right response -> Right response

