{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module AppSpec where

import           Data.String.Conversions
import           Network.Wai.Test (SResponse)
import           Test.Hspec hiding (pending)
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

import           App

spec = with app $ do
  describe "/ips" $ do
    it "returns an empty list" $ do
      get "/ips" `shouldRespondWith` [json|[]|]

  describe "POST /new-ip" $ do
    it "allows to add ips" $ do
      postJSON "/new-ip" "\"192.1.2.3\"" `shouldRespondWith` 204
      get "/ips" `shouldRespondWith` [json|["192.1.2.3"]|]

postJSON :: SBS -> LBS -> WaiSession SResponse
postJSON url body =
  request "POST" url [("Content-Type", "application/json")] body
