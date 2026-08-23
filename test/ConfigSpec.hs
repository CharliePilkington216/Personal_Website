{-# LANGUAGE OverloadedStrings #-}

module ConfigSpec (spec) where

import Config
import Test.Hspec

spec :: Spec
spec = do
  describe "loadConfig" $ do
    it "loads the configuration from the environment variables" $ do
      config <- loadConfig
      authDbConnString config `shouldBe` "postgresql://testauth_admin:password@localhost:5432/testauthdb"
      portfolioDbConnString config `shouldBe` "postgres://testportfolio_admin:password@localhost:5432/testportfoliodb"
      inquiryDbConnString config `shouldBe` "postgres://testinquiry_admin:password@localhost:5432/testinquirydb"
      jwtSecret config `shouldBe` "test-secret-do-not-use-in-production"
      domain config `shouldBe` "localhost"
      resendApiKey config `shouldBe` "super-secret-api-key"
      fromEmail config `shouldBe` "sender@example.com"
      notifyEmail config `shouldBe` "receiver@example.com"