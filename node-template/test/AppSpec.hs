{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module AppSpec where

import           Test.Hspec hiding (pending)
import           Test.Hspec.Wai

import           App

spec :: Spec
spec = with app $ do
  describe "something" $ do
    it "does stuff" $ do
      pending
