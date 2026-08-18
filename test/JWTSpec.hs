{-# LANGUAGE OverloadedStrings #-}

module JWTSpec (spec) where

import Data.Either (isLeft)
import qualified Data.ByteString.Char8 as BSC
import Network.Wai
import Servant
import Servant.Server.Experimental.Auth (unAuthHandler) 
import Data.Text (Text)
import qualified Data.Text as T
import JWT 
import Test.Hspec

adminRequest :: Text -> Request
adminRequest token =
    defaultRequest
      { requestHeaders =
          [ ("Authorization", BSC.pack ("Bearer " <> T.unpack token))
          ]
      }

refreshRequest :: Text -> Request
refreshRequest token =
    defaultRequest
      { requestHeaders =
          [ ("Cookie", BSC.pack ("refresh_token=" <> T.unpack token))
          ]
      }

testSecret, testDomain, testAdminId, testSessionId :: Text
testSecret   = "test-jwt-secret-for-unit-tests-only"
testDomain   = "example.com"
testAdminId  = "3fa85f64-5717-4562-b3fc-2c963f66afa6"
testSessionId = "9c858901-8a57-4791-81fe-4c455b099bc9"

spec :: Spec
spec = do
  describe "createAccessToken / verifyAccessToken" $ do
    it "round-trips: a created token verifies back to the same admin/session ids" $ do
      Right token <- createAccessToken testSecret testDomain testAdminId testSessionId 
      verified <- verifyAccessToken testSecret testDomain token
      verified `shouldBe` Right (testAdminId, testSessionId)

    it "produces a well-formed compact JWT (three dot-separated segments)" $ do
      Right token <- createAccessToken testSecret testDomain testAdminId testSessionId
      length (T.splitOn "." token) `shouldBe` 3

    it "rejects verification against the wrong secret" $ do
      Right token <- createAccessToken testSecret testDomain testAdminId testSessionId
      verified <- verifyAccessToken "a-completely-different-secret" testDomain token
      verified `shouldSatisfy` isLeft

    it "rejects verification against the wrong domain" $ do
      Right token <- createAccessToken testSecret testDomain testAdminId testSessionId
      verified <- verifyAccessToken testSecret "not-my-domain.uk" token
      verified `shouldSatisfy` isLeft

    it "rejects a tampered token" $ do
      Right token <- createAccessToken testSecret testDomain testAdminId testSessionId
      let tampered = T.dropEnd 1 token <> "x"
      verified <- verifyAccessToken testSecret testDomain tampered
      verified `shouldSatisfy` isLeft

  describe "createRefreshToken / verifyRefreshToken" $
    it "round-trips the same way as access tokens" $ do
      Right token <- createRefreshToken testSecret testDomain testAdminId testSessionId
      verified <- verifyRefreshToken testSecret testDomain token
      verified `shouldBe` Right (testAdminId, testSessionId)

  describe "token type enforcement" $ do
    it "rejects an access token where a refresh token is expected" $ do
      Right accessToken <- createAccessToken testSecret testDomain testAdminId testSessionId
      verified <- verifyRefreshToken testSecret testDomain accessToken
      verified `shouldSatisfy` isLeft

    it "rejects a refresh token where an access token is expected" $ do
      Right refreshToken <- createRefreshToken testSecret testDomain testAdminId testSessionId
      verified <- verifyAccessToken testSecret testDomain refreshToken
      verified `shouldSatisfy` isLeft

  describe "adminAuthHandler" $ do

    it "returns the admin/session ids when given a valid access token" $ do
      Right accessToken <-
        createAccessToken
          testSecret
          testDomain
          testAdminId
          testSessionId

      let authHandler =
            adminAuthHandler
              (verifyAccessToken testSecret testDomain)

      result <-
        runHandler $
          unAuthHandler authHandler (adminRequest accessToken)

      result `shouldBe`
        Right (testAdminId, testSessionId)


    it "returns an error when given an invalid access token" $ do
      let authHandler =
            adminAuthHandler
              (verifyAccessToken testSecret testDomain)

      result <-
        runHandler $
          unAuthHandler authHandler (adminRequest "invalid-token")

      result `shouldSatisfy` isLeft


    it "rejects a request without an Authorization header" $ do
      let authHandler =
            adminAuthHandler
              (verifyAccessToken testSecret testDomain)

      result <-
        runHandler $
          unAuthHandler authHandler defaultRequest

      result `shouldSatisfy` isLeft


  describe "refreshAuthHandler" $ do

    it "returns the refresh token and admin/session ids when given a valid refresh token" $ do
      Right refreshToken <-
        createRefreshToken
          testSecret
          testDomain
          testAdminId
          testSessionId

      let authHandler =
            refreshAuthHandler
              (verifyRefreshToken testSecret testDomain)

      result <-
        runHandler $
          unAuthHandler authHandler (refreshRequest refreshToken)

      result `shouldBe`
        Right
          ( refreshToken
          , testAdminId
          , testSessionId
          )


    it "returns an error when given an invalid refresh token" $ do
      let authHandler =
            refreshAuthHandler
              (verifyRefreshToken testSecret testDomain)

      result <-
        runHandler $
          unAuthHandler authHandler (refreshRequest "invalid-token")

      result `shouldSatisfy` isLeft


    it "rejects a request without a refresh_token cookie" $ do
      let authHandler =
            refreshAuthHandler
              (verifyRefreshToken testSecret testDomain)

      let request =
            defaultRequest
              { requestHeaders =
                  [ ("Cookie", "some_other_cookie=value")
                  ]
              }

      result <-
        runHandler $
          unAuthHandler authHandler request

      result `shouldSatisfy` isLeft