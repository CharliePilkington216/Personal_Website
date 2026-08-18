{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- module to organise the /auth section of the API
module Authorization (authServer) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Servant
import Web.Cookie (SetCookie)

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


-- auth server definition

authServer :: Server AuthAPI
authServer = loginHandler :<|> refreshHandler :<|> logoutHandler

-- endpoint definitions

loginHandler :: LogInRequest -> Handler (Headers '[Header "Set-Cookie" SetCookie] LogInResponse)
loginHandler = undefined

refreshHandler :: Text -> Handler (Headers '[Header "Set-Cookie" SetCookie] RefreshResponse)
refreshHandler = undefined

logoutHandler :: Text -> Handler (Headers '[Header "Set-Cookie" SetCookie] NoContent)
logoutHandler = undefined