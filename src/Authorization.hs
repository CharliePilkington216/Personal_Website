{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- module to organise the /auth section of the API
module Authorization (AuthAPI, authServer) where

import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Servant
import Servant.Server.Experimental.Auth
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
                :> AuthProtect "refresh-auth"
                :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie] RefreshResponse)
          :<|> "logout" 
                :> AuthProtect "refresh-auth"
                :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie] NoContent)


-- auth server definition

type instance AuthServerData (AuthProtect "refresh-auth") = (Text, Text, Text)

authServer :: Server AuthAPI
authServer = loginHandler :<|> refreshHandler :<|> logoutHandler

-- endpoint definitions

loginHandler :: LogInRequest -> Handler (Headers '[Header "Set-Cookie" SetCookie] LogInResponse)
loginHandler = undefined

refreshHandler :: (Text, Text, Text) -> Handler (Headers '[Header "Set-Cookie" SetCookie] RefreshResponse)
refreshHandler = undefined

logoutHandler :: (Text, Text, Text) -> Handler (Headers '[Header "Set-Cookie" SetCookie] NoContent)
logoutHandler = undefined