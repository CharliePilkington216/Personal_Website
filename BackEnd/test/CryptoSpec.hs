{-# LANGUAGE OverloadedStrings #-}

module CryptoSpec (spec) where

import Crypto (checkPassword, hashToken, saltedHashPassword)
import Data.Maybe (fromJust, isJust)
import Test.Hspec

spec :: Spec
spec = do
  describe "saltedHashPassword" $ do
    it "creates 2 different hashes for the same password" $ do
      mHash1 <- saltedHashPassword "password"
      mHash2 <- saltedHashPassword "password"
      mHash1 `shouldSatisfy` isJust
      mHash2 `shouldSatisfy` isJust
      mHash1 `shouldNotBe` mHash2

  describe "checkPassword" $ do
    it "returns true if password matches hash" $ do
      mHash <- saltedHashPassword "password"
      mHash `shouldSatisfy` isJust
      checkPassword (fromJust mHash) "password" `shouldBe` True

    it "returns false if password does not match hash" $ do
      mHash <- saltedHashPassword "password"
      mHash `shouldSatisfy` isJust
      checkPassword (fromJust mHash) "wrong-password" `shouldBe` False

  describe "hashToken" $ do
    it "creates 2 identical hashes for the same token" $ do
      hashToken "some-refresh-token-value" `shouldBe` hashToken "some-refresh-token-value"