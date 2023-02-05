{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Janus (waiapp)
import Janus.Data.Config (Config(..))
import Network.HTTP.Types.Header
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

config = Config {issuer = "testissuer", key = "testkey", valid = 600}

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (waiapp config) $ do
  describe "GET /static/favicon.ico" $ do
    it "responds with 200" $ do
      get "/static/favicon.ico" `shouldRespondWith` 200
