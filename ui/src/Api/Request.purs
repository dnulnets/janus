module Janus.Api.Request
  ( Token
  , BaseURL(..)
  , RequestMethod(..)
  , RequestTemplate(..)
  , defaultRequest
  , LoginFields(..)
  , login
  , readToken
  , writeToken
  , removeToken
  ) where

import Prelude

import Affjax.RequestBody as RB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Affjax.Web (Request, printError, request)
import Janus.Api.Endpoint (Endpoint(..), endpointCodec)
import Janus.Data.Profile (Profile)
import Janus.Data.Profile as Profile
import Janus.Data.Username (Username)
import Janus.Data.Username as Username
import Data.Argonaut.Core (Json)
import Data.Bifunctor (lmap)
import Data.Codec as Codec
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError, printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Routing.Duplex (print)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)

-- |The java web token used for the authorization of the REST calls to the api.
newtype Token = Token String
derive instance eqToken :: Eq Token
derive instance ordToken :: Ord Token
instance showToken :: Show Token where
  show (Token _) = "Token {- token -}"

-- |Holds a URL that are used by the REST calls to the api. 
newtype BaseURL = BaseURL String

-- |The possible types of requests to the api and their payloads.
data RequestMethod
  = Get
  | Post (Maybe Json)
  | Put (Maybe Json)
  | Delete

-- |The template for a request. It contains the method, payload and endpoint.
type RequestTemplate =
  { endpoint :: Endpoint
  , method :: RequestMethod
  }

-- |The default request for a specific url, template and token.
defaultRequest :: BaseURL -> Maybe Token -> RequestTemplate -> Request Json
defaultRequest (BaseURL baseUrl) auth { endpoint, method } =
  { method: Left requestMethod
  , url: baseUrl <> print endpointCodec endpoint
  , headers: case auth of
      Nothing -> []
      Just (Token t) -> [ RequestHeader "Authorization" $ "Bearer " <> t ]
  , content: RB.json <$> body
  , username: Nothing
  , password: Nothing
  , timeout: Nothing
  , withCredentials: false
  , responseFormat: RF.json
  }
  where
  Tuple requestMethod body = case method of
    Get -> Tuple GET Nothing
    Post b -> Tuple POST b
    Put b -> Tuple PUT b
    Delete -> Tuple DELETE Nothing

-- | The fields needed for a login.
type LoginFields =
  { username :: Username
  , password :: String
  }

-- | The codec for encoding and decoding of LoginFields.
loginCodec :: JsonCodec LoginFields
loginCodec =
  CAR.object "LoginRequest"
    { username: Username.codec
    , password: CA.string
    }

-- | Logs in the user and returns with a profile and token.
login :: forall m. MonadAff m => BaseURL -> LoginFields -> m (Either String (Tuple Token Profile))
login baseUrl fields =
  let
    method = Post $ Just $ Codec.encode (CAR.object "User" { user: loginCodec }) { user: fields }
  in
    requestUser baseUrl { endpoint: Login, method }

-- | Performs the actual request and returns with a profile and token.
requestUser :: forall m. MonadAff m => BaseURL -> RequestTemplate -> m (Either String (Tuple Token Profile))
requestUser baseUrl opts = do
  res <- liftAff $ request $ defaultRequest baseUrl Nothing opts
  case res of
    Left e -> pure $ Left $ printError e
    Right v -> pure $ lmap printJsonDecodeError $ decodeAuthProfile =<< Codec.decode (CAR.object "User" { user: CA.json }) v.body

-- |Decodes the response from the login request and returns with the token and profile.
decodeAuthProfile :: { user :: Json } -> Either JsonDecodeError (Tuple Token Profile)
decodeAuthProfile { user } = do
  { token } <- Codec.decode (CAR.object "Token" { token: tokenCodec }) user
  profile <- Codec.decode Profile.profileCodec user
  pure (Tuple token profile)
  where
  tokenCodec =
    CA.prismaticCodec "Token (inner)" (Just <<< Token) (\(Token t) -> t) CA.string

-- |The key used in the broswers local storage for storing the token
tokenKey = "token" :: String

-- |Reads the token from local storage.
readToken :: Effect (Maybe Token)
readToken = do
  str <- getItem tokenKey =<< localStorage =<< window
  pure $ map Token str

-- |Writes the token to local storage.
writeToken :: Token -> Effect Unit
writeToken (Token str) =
  setItem tokenKey str =<< localStorage =<< window

-- |Removes the token from local storage.
removeToken :: Effect Unit
removeToken =
  removeItem tokenKey =<< localStorage =<< window
