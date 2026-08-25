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
import Logger (newLogger, closeLogger)

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

testSecret, testDomain, testAdminId, testSessionId, expiredAccessToken, expiredRefreshToken :: Text
testSecret   = "test-jwt-secret-for-unit-tests-only"
testDomain   = "example.com"
testAdminId  = "3fa85f64-5717-4562-b3fc-2c963f66afa6"
testSessionId = "9c858901-8a57-4791-81fe-4c455b099bc9"
expiredAccessToken = "eyJhbGciOiJIUzI1NiJ9.eyJhdWQiOiJleGFtcGxlLmNvbSIsImV4cCI6MS43ODcwODIwOTYxMjgwODE1NzNlOSwiaWF0IjoxLjc4NzA4MTE5NjEyODA4MTU3M2U5LCJpc3MiOiJleGFtcGxlLmNvbSIsInNlc3Npb24iOiI5Yzg1ODkwMS04YTU3LTQ3OTEtODFmZS00YzQ1NWIwOTliYzkiLCJzdWIiOiIzZmE4NWY2NC01NzE3LTQ1NjItYjNmYy0yYzk2M2Y2NmFmYTYiLCJ0eXBlIjoiYWNjZXNzIn0.14HTPIByePY7k5WuhGs1-NNsuFu3FCt5lPDyGaZVrs4"
expiredRefreshToken = "eyJhbGciOiJIUzI1NiJ9.eyJhdWQiOiJleGFtcGxlLmNvbSIsImV4cCI6MS43ODcwNzc4MTA3NDY3MDQ0N2U5LCJpYXQiOjEuNzg2NDczMDEwNzQ2NzA0NDdlOSwiaXNzIjoiZXhhbXBsZS5jb20iLCJzZXNzaW9uIjoiOWM4NTg5MDEtOGE1Ny00NzkxLTgxZmUtNGM0NTViMDk5YmM5Iiwic3ViIjoiM2ZhODVmNjQtNTcxNy00NTYyLWIzZmMtMmM5NjNmNjZhZmE2IiwidHlwZSI6InJlZnJlc2gifQ.SvJn6RDN8BT684ckCuEo7_5xu-M2EpicBiJkDwI1Mfk"

spec :: Spec
spec = do
  logger <- runIO $ newLogger "logs/test.log"

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

    it "rejects an expired token" $ do
      verified <- verifyAccessToken testSecret testDomain expiredAccessToken
      verified `shouldSatisfy` isLeft

  describe "createRefreshToken / verifyRefreshToken" $ do
    it "round-trips the same way as access tokens" $ do
      Right token <- createRefreshToken testSecret testDomain testAdminId testSessionId
      verified <- verifyRefreshToken testSecret testDomain token
      verified `shouldBe` Right (testAdminId, testSessionId)

    it "rejects an expired token" $ do
      verified <- verifyRefreshToken testSecret testDomain expiredRefreshToken
      verified `shouldSatisfy` isLeft

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
            adminAuthHandler logger
              (verifyAccessToken testSecret testDomain)

      result <-
        runHandler $
          unAuthHandler authHandler (adminRequest accessToken)

      result `shouldBe`
        Right (testAdminId, testSessionId)


    it "returns an error when given an invalid access token" $ do
      let authHandler =
            adminAuthHandler logger
              (verifyAccessToken testSecret testDomain)

      result <-
        runHandler $
          unAuthHandler authHandler (adminRequest "invalid-token")

      result `shouldSatisfy` isLeft


    it "rejects a request without an Authorization header" $ do
      let authHandler =
            adminAuthHandler logger
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
            refreshAuthHandler logger
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
            refreshAuthHandler logger
              (verifyRefreshToken testSecret testDomain)

      result <-
        runHandler $
          unAuthHandler authHandler (refreshRequest "invalid-token")

      result `shouldSatisfy` isLeft


    it "rejects a request without a refresh_token cookie" $ do
      let authHandler =
            refreshAuthHandler logger
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

  runIO $ closeLogger logger