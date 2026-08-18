{-# LANGUAGE OverloadedStrings #-}

module ConfigSpec (spec) where

import Config
import Test.Hspec

spec :: Spec
spec = do
  describe "loadConfig" $ do
    it "loads the configuration from the environment variables" $ do
      config <- loadConfig
      authDbConnString config `shouldBe` "postgres://user:password@localhost:5432/authdb"
      portfolioDbConnString config `shouldBe` "postgres://user:password@localhost:5432/portfoliodb"
      inquiryDbConnString config `shouldBe` "postgres://user:password@localhost:5432/inquirydb"
      jwtSecret config `shouldBe` "test-secret"
      domain config `shouldBe` "localhost"