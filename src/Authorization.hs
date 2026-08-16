{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- module to organise the /auth section of the api
module Authorization where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Servant
import Web.Cookie (SetCookie)

import Crypto.BCrypt
  ( hashPasswordUsingPolicy
  , slowerBcryptHashingPolicy
  , validatePassword
  )
import Crypto.Hash (SHA256 (..), hashWith)
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE


-- Request JSON definitions

data LogInRequest = LogInRequest
  { email :: Text
  , password :: Text
  } deriving (Generic) 

instance FromJSON LogInRequest

-- Response JSON defintions

data AccessToken = AccessToken
  { token :: Text
  } deriving (Generic)

instance ToJSON AccessToken

type LogInResponse = AccessToken

type RefreshResponse = AccessToken


-- API type definition

type AuthAPI = "login" 
                :> ReqBody '[JSON] LogInRequest 
                :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie] LogInResponse)
          :<|> "refresh" 
                :> Header' '[Required, Strict] "Cookie" Text 
                :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie] RefreshResponse)
          :<|> "logout" 
                :> Header' '[Required, Strict] "Cookie" Text 
                :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie] NoContent)


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