{-# LANGUAGE OverloadedStrings #-}

module JWTSpec (spec) where

import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as T
import JWT (createAccessToken, createRefreshToken, verifyAccessToken, verifyRefreshToken)
import Test.Hspec

testSecret, testDomain, testAdminId, testSessionId :: Text
testSecret   = "test-jwt-secret-for-unit-tests-only"
testDomain   = "example.com"
testAdminId  = "3fa85f64-5717-4562-b3fc-2c963f66afa6"
testSessionId = "9c858901-8a57-4791-81fe-4c455b099bc9"

spec :: Spec
spec = do
  describe "createAccessToken / verifyAccessToken" $ do
    it "round-trips: a created token verifies back to the same admin/session ids" $ do
      Right token <- createAccessToken testAdminId testSessionId testSecret testDomain
      verified <- verifyAccessToken token testSecret testDomain
      verified `shouldBe` Right (testAdminId, testSessionId)

    it "produces a well-formed compact JWT (three dot-separated segments)" $ do
      Right token <- createAccessToken testAdminId testSessionId testSecret testDomain
      length (T.splitOn "." token) `shouldBe` 3

    it "rejects verification against the wrong secret" $ do
      Right token <- createAccessToken testAdminId testSessionId testSecret testDomain
      verified <- verifyAccessToken token "a-completely-different-secret" testDomain
      verified `shouldSatisfy` isLeft

    it "rejects verification against the wrong domain" $ do
      Right token <- createAccessToken testAdminId testSessionId testSecret testDomain
      verified <- verifyAccessToken token testSecret "not-my-domain.uk"
      verified `shouldSatisfy` isLeft

    it "rejects a tampered token" $ do
      Right token <- createAccessToken testAdminId testSessionId testSecret testDomain
      let tampered = T.dropEnd 1 token <> "x"
      verified <- verifyAccessToken tampered testSecret testDomain
      verified `shouldSatisfy` isLeft

  describe "createRefreshToken / verifyRefreshToken" $
    it "round-trips the same way as access tokens" $ do
      Right token <- createRefreshToken testAdminId testSessionId testSecret testDomain
      verified <- verifyRefreshToken token testSecret testDomain
      verified `shouldBe` Right (testAdminId, testSessionId)

  describe "token type enforcement" $ do
    it "rejects an access token where a refresh token is expected" $ do
      Right accessToken <- createAccessToken testAdminId testSessionId testSecret testDomain
      verified <- verifyRefreshToken accessToken testSecret testDomain
      verified `shouldSatisfy` isLeft

    it "rejects a refresh token where an access token is expected" $ do
      Right refreshToken <- createRefreshToken testAdminId testSessionId testSecret testDomain
      verified <- verifyAccessToken refreshToken testSecret testDomain
      verified `shouldSatisfy` isLeft