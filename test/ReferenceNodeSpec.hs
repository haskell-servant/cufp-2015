{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ReferenceNodeSpec where

import           Data.String.Conversions
import           Network.Wai.Test (SResponse)
import           Test.Hspec hiding (pending)
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

import           ReferenceNode

spec :: Spec
spec = with app $ do
  describe "/nodes" $ do
    it "returns an empty list" $ do
      get "/nodes" `shouldRespondWith` [json|[]|]

  describe "POST /nodes/new" $ do
    it "allows to add ips" $ do
      postJSON "/nodes/new" "{\"host\": \"192.1.2.3\", \"port\": 8080}"
        `shouldRespondWith` 204 {matchBody = Just ""}
      get "/nodes" `shouldRespondWith`
        [json|[{host: "192.1.2.3", port: 8080}]|]

postJSON :: SBS -> LBS -> WaiSession SResponse
postJSON url body =
  request "POST" url [("Content-Type", "application/json")] body
