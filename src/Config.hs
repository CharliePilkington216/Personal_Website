{-# LANGUAGE OverloadedStrings #-}

-- module to load in environment variables
module Config (Config, authDbConnString, portfolioDbConnString, inquiryDbConnString, jwtSecret, loadConfig) where

import Configuration.Dotenv (loadFile, defaultConfig)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import System.Exit (die)

data Config = Config
  { authDbConnString      :: Text
  , portfolioDbConnString :: Text
  , inquiryDbConnString   :: Text
  , jwtSecret             :: Text
  }

-- will crash the program if this fails to run successfully
loadConfig :: IO Config
loadConfig = do
  _ <- loadFile defaultConfig
  Config
    <$> require "AUTHDB_CONN_STRING"
    <*> require "PORTFOLIODB_CONN_STRING"
    <*> require "INQUIRYDB_CONN_STRING"
    <*> require "JWT_SECRET"
  where
    require :: String -> IO Text
    require key = do
      mVal <- lookupEnv key
      case mVal of
        Just val | not (null val) -> pure (T.pack val)
        _ -> die ("Missing required environment variable: " <> key)