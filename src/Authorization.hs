{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- module to organise the /auth section of the API
module Authorization (AuthAPI, TokenMinter, LogInRequest (..), AccessToken (..), authServer, loginHandler, refreshHandler, logoutHandler) where

import Crypto (checkPassword, hashToken)
import Crypto.JWT (JWTError)
import Database (DB)
import Data.Pool (withResource)
import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Encoding as TE
import Database.PostgreSQL.Simple (Connection, Only (..), query, execute)
import GHC.Generics (Generic)
import Servant
import Servant.Server.Experimental.Auth
import Web.Cookie
    ( SetCookie (..)
    , defaultSetCookie
    , sameSiteStrict
    )
import JWT
import GHC.Int (Int64)

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
                (Right accessToken, Right refreshToken) -> do
                    insertResult <- liftIO (try (withResource db (insertToken sessionId refreshToken))
                                                :: IO (Either SomeException Int64))
                    case insertResult of
                        Left _  -> throwError err500
                        Right _ -> pure (addHeader (refreshCookie refreshToken) (AccessToken accessToken))
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

-- takes the refresh_token, admin_id, session_id from the verified refresh token
-- checks whether the session is with that admin, not revoked and not expired
-- checks the refresh token is not expired and belongs to that session, if not it revokes that session
-- if successful it revokes the refresh token and generates a new refresh token and access token to give to the user
refreshHandler :: DB -> TokenMinter -> TokenMinter -> (Text, Text, Text) -> Handler (Headers '[Header "Set-Cookie" SetCookie] RefreshResponse)
refreshHandler db mkAccessToken mkRefreshToken (refreshToken, adminId, sessionId) = do
    validation <- liftIO (try (withResource db (validateAndRotateToken adminId sessionId refreshToken))
                                :: IO (Either SomeException Bool))
    case validation of
        Left _      -> throwError err500
        Right False -> throwError err401
        Right True  -> pure ()

    accessResult  <- liftIO (mkAccessToken adminId sessionId)
    refreshResult <- liftIO (mkRefreshToken adminId sessionId)

    case (accessResult, refreshResult) of
        (Right newAccessToken, Right newRefreshToken) -> do
            insertResult <- liftIO (try (withResource db (insertToken sessionId newRefreshToken))
                                        :: IO (Either SomeException Int64))
            case insertResult of
                Left _  -> throwError err500
                Right _ -> pure (addHeader (refreshCookie newRefreshToken) (AccessToken newAccessToken))
        _ ->
            throwError err500
  where
    -- Checks the session belongs to this admin and is still live, then checks the presented refresh token against its stored hash for that session:
    --   * session missing/mismatched/revoked/expired -> False
    --   * token hash not found for this session       -> False
    --   * token hash found but already revoked         -> revoke the whole session (reuse of a rotated-out token), False
    --   * token hash found and still live               -> revoke just that token (it's being rotated), True
    validateAndRotateToken :: Text -> Text -> Text -> Connection -> IO Bool
    validateAndRotateToken adminId' sessionId' refreshToken' conn = do
        sessionRows <- query conn
            "SELECT 1 FROM sessions \
            \WHERE session_id = ? AND admin_id = ? AND revoked = false AND expires_at > now()"
            (sessionId', adminId') :: IO [Only Int]

        case sessionRows of
            [] -> pure False
            _  -> do
                let tokenHash = hashToken refreshToken'
                tokenRows <- query conn
                    "SELECT revoked FROM tokens WHERE token_hash = ? AND session_id = ?"
                    (tokenHash, sessionId') :: IO [Only Bool]

                case tokenRows of
                    [] ->
                        pure False

                    Only True : _ -> do
                        _ <- execute conn
                            "UPDATE sessions SET revoked = true WHERE session_id = ?"
                            (Only sessionId')
                        pure False

                    Only False : _ -> do
                        _ <- execute conn
                            "UPDATE tokens SET revoked = true WHERE token_hash = ?"
                            (Only tokenHash)
                        pure True

-- takes the refresh_token, admin_id, session_id from the verified refresh token in cookies
-- revokes the session which matches the given session_id and admin_id
-- sends back an empty cookies and no content to the user
logoutHandler :: DB -> (Text, Text, Text) -> Servant.Handler (Headers '[Header "Set-Cookie" SetCookie] NoContent)
logoutHandler db (_refreshToken, adminId, sessionId) = do
    result <- liftIO (try (withResource db (revokeSession adminId sessionId))
                          :: IO (Either SomeException Int64))
    case result of
        Left _  -> throwError err500
        Right _ -> pure (addHeader clearRefreshCookie NoContent)
  where
    revokeSession :: Text -> Text -> Connection -> IO Int64
    revokeSession adminId' sessionId' conn =
        execute conn
            "UPDATE sessions SET revoked = true WHERE session_id = ? AND admin_id = ?"
            (sessionId', adminId')

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

clearRefreshCookie :: SetCookie
clearRefreshCookie = defaultSetCookie
    { setCookieName     = "refresh_token"
    , setCookieValue    = ""
    , setCookiePath     = Just "/"
    , setCookieHttpOnly = True
    , setCookieSecure   = True
    , setCookieSameSite = Just sameSiteStrict
    , setCookieMaxAge   = Just 0
    }

insertToken :: Text -> Text -> Connection -> IO Int64
insertToken sessionId' refreshToken' conn =
    execute conn
        "INSERT INTO tokens (token_hash, session_id) VALUES (?, ?)"
        (hashToken refreshToken', sessionId')