{-# LANGUAGE OverloadedStrings #-}

-- module to ensure passwords and tokens are never stored in plaintext or exposed by db leak
module Crypto (saltedHashPassword, checkPassword, hashToken) where

import Crypto.BCrypt
  ( hashPasswordUsingPolicy
  , slowerBcryptHashingPolicy
  , validatePassword
  )
import Crypto.Hash (SHA256 (..), hashWith)
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

-- Functions to ensure password and token security

saltedHashPassword :: Text -> IO (Maybe Text)
saltedHashPassword plaintext = do
  mHash <- hashPasswordUsingPolicy slowerBcryptHashingPolicy (TE.encodeUtf8 plaintext)
  pure (TE.decodeUtf8 <$> mHash)

checkPassword :: Text -> Text -> Bool
checkPassword storedHash plaintext =
  validatePassword (TE.encodeUtf8 storedHash) (TE.encodeUtf8 plaintext)

hashToken :: Text -> Text
hashToken plaintext =
  TE.decodeUtf8
    . convertToBase Base16
    . hashWith SHA256
    . TE.encodeUtf8
    $ plaintext