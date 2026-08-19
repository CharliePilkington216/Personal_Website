{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- module to organise the /auth section of the API
module Authorization (AuthAPI, authServer) where

import Crypto (checkPassword)
import Crypto.JWT (JWTError)
import Database (DB)
import Data.Pool (withResource)
import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple (Connection, Only (..), query)
import GHC.Generics (Generic)
import Servant
import Servant.Server.Experimental.Auth
import Web.Cookie
    ( SetCookie (..)
    , defaultSetCookie
    , sameSiteStrict
    )
import JWT

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

-- type definition for the token creation functions

type TokenMinter = Text -> Text -> IO (Either JWTError Text)

-- auth server definition

type instance AuthServerData (AuthProtect "refresh-auth") = (Text, Text, Text)

authServer :: DB -> TokenMinter -> TokenMinter -> Server AuthAPI
authServer db mkAccessToken mkRefreshToken = loginHandler db mkAccessToken mkRefreshToken 
                                        :<|> refreshHandler db mkAccessToken mkRefreshToken 
                                        :<|> logoutHandler db

-- endpoint definitions

-- take a LogInRequest JSON
-- get the admin_id and password_hash of the given email from the db
-- check the given password matches the password_hash
-- if correct create a new session linked to the admin and return the session_id
-- use the admin_id and session_id to return a 200 OK with access token in JSON and refresh token in Cookies
-- throw 401 for incorrect credentials or 500 for any server error
loginHandler :: DB -> TokenMinter -> TokenMinter -> LogInRequest -> Servant.Handler (Headers '[Header "Set-Cookie" SetCookie] LogInResponse)
loginHandler db mkAccessToken mkRefreshToken req = do
    lookupResult <- liftIO (try (withResource db lookupAdmin) :: IO (Either SomeException [(Text, Text)]))
    (adminId, storedHash) <- case lookupResult of
        Left _          -> throwError err500
        Right []        -> throwError err401
        Right (row : _) -> pure row

    if not (checkPassword storedHash (password req))
        then throwError err401
        else do
            sessionResult <- liftIO (try (withResource db (insertSession adminId)) :: IO (Either SomeException [Only Text]))
            sessionId <- case sessionResult of
                Left _             -> throwError err500
                Right []           -> throwError err500
                Right (Only s : _) -> pure s

            accessResult  <- liftIO (mkAccessToken adminId sessionId)
            refreshResult <- liftIO (mkRefreshToken adminId sessionId)

            case (accessResult, refreshResult) of
                (Right accessToken, Right refreshToken) -> pure (addHeader (refreshCookie refreshToken) (AccessToken accessToken))
                _ -> throwError err500
  where
    lookupAdmin :: Connection -> IO [(Text, Text)]
    lookupAdmin conn =
        query conn
            "SELECT admin_id::text, password_hash FROM admins WHERE email = ?"
            (Only (email req))

    insertSession :: Text -> Connection -> IO [Only Text]
    insertSession adminId conn =
        query conn
            "INSERT INTO sessions (admin_id) VALUES (?) RETURNING session_id::text"
            (Only adminId)

refreshHandler :: DB -> TokenMinter -> TokenMinter -> (Text, Text, Text) -> Servant.Handler (Headers '[Header "Set-Cookie" SetCookie] RefreshResponse)
refreshHandler db mkAccessToken mkRefreshToken = undefined

logoutHandler :: DB -> (Text, Text, Text) -> Servant.Handler (Headers '[Header "Set-Cookie" SetCookie] NoContent)
logoutHandler db = undefined

-- helper functions

refreshCookie :: Text -> SetCookie
refreshCookie refreshToken = defaultSetCookie
    { setCookieName     = "refresh_token"
    , setCookieValue    = TE.encodeUtf8 refreshToken
    , setCookiePath     = Just "/"
    , setCookieHttpOnly = True
    , setCookieSecure   = True
    , setCookieSameSite = Just sameSiteStrict
    , setCookieMaxAge = Just (realToFrac refreshTokenLifeTime)
    }