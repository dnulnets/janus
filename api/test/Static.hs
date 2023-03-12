{-# LANGUAGE OverloadedStrings #-}

module Static (spec) where

import Test.Hspec ( describe, it, Example(Arg), SpecWith )
import Test.Hspec.Wai ( get, shouldRespondWith, WaiExpectation )

-- | Test that the static files are served
spec :: SpecWith (Arg (WaiExpectation st))
spec = describe "Static files" $ do
    it "favicon responds with 200" $ do
      get "/static/favicon.ico" `shouldRespondWith` 200
    it "index.html responds with 200" $ do
      get "/" `shouldRespondWith` 200
    it "index.js responds with 200" $ do
      get "/index.js" `shouldRespondWith` 200
