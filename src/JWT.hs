{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}

-- module to handle the creation and verification of JWTs for authentication
module JWT (createAccessToken, createRefreshToken, verifyAccessToken, verifyRefreshToken, authContext) where

import Control.Lens ((&), (.~), (^.), preview, review)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Crypto.JOSE.JWK (JWK, fromOctets)
import Crypto.JWT
  ( ClaimsSet
  , HasClaimsSet (..)
  , Audience (..)
  , JWTError
  , NumericDate (..)
  , bestJWSAlg
  , claimAud
  , claimExp
  , claimIat
  , claimIss
  , claimSub
  , decodeCompact
  , defaultJWTValidationSettings
  , emptyClaimsSet
  , encodeCompact
  , issuerPredicate
  , newJWSHeader
  , signJWT
  , stringOrUri
  , verifyJWT
  )
import Crypto.Random.Types (MonadRandom (..))
import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , Value (Object, String)
  , withObject
  , (.:)
  )
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Time (NominalDiffTime, addUTCTime, getCurrentTime)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Network.Wai (Request, requestHeaders)
import Servant
import Servant.Server.Experimental.Auth

-- Local orphan instance: this version of crypton doesn't provide a generic
-- "lift MonadRandom through any transformer" instance, so ExceptT JWTError IO
-- doesn't get MonadRandom for free even though its base monad (IO) has it.
-- signJWT needs MonadRandom (for key/nonce generation during signing), so
-- we lift it explicitly.
instance MonadRandom (ExceptT JWTError IO) where
  getRandomBytes = lift . getRandomBytes

-- token types definition

data TokenType = AccessType | RefreshType
  deriving (Eq, Show)

-- expiration times for tokens

refreshTokenLifeTime :: NominalDiffTime
refreshTokenLifeTime = 7 * 24 * 60 * 60

accessTokenLifeTime :: NominalDiffTime
accessTokenLifeTime = 15 * 60

-- functions to get required JWT fields

tokenTypeText :: TokenType -> Text
tokenTypeText AccessType  = "access"
tokenTypeText RefreshType = "refresh"

tokenLifetime :: TokenType -> NominalDiffTime
tokenLifetime AccessType  = accessTokenLifeTime
tokenLifetime RefreshType = refreshTokenLifeTime

mkSigningKey :: Text -> JWK
mkSigningKey secret = fromOctets (TE.encodeUtf8 secret)

-- ============================================================
-- ClaimsSet subtype carrying our two non-standard claims,
-- "type" and "session", as real typed fields.
-- ============================================================

data TokenClaims = TokenClaims
  { tcClaimsSet :: ClaimsSet
  , tcType      :: Text
  , tcSession   :: Text
  } deriving (Eq, Show)

instance HasClaimsSet TokenClaims where
  claimsSet f tc = fmap (\cs' -> tc { tcClaimsSet = cs' }) (f (tcClaimsSet tc))

instance FromJSON TokenClaims where
  parseJSON v = withObject "TokenClaims" (\o ->
    TokenClaims <$> parseJSON v
                <*> o .: "type"
                <*> o .: "session") v

instance ToJSON TokenClaims where
  toJSON (TokenClaims cs t s) =
    case toJSON cs of
      Object o -> Object (KeyMap.insert "type" (String t) (KeyMap.insert "session" (String s) o))
      other    -> other

-- Runs a JOSE action (whose errors are thrown via MonadError/AsError,
-- rather than returned as an Either) inside our String-based ExceptT,
-- collapsing any failure into the given message.
runJOSE :: String -> ExceptT JWTError IO a -> ExceptT String IO a
runJOSE msg action = do
  outcome <- liftIO (runExceptT action)
  either (const (throwError msg)) pure outcome

-- Build and sign a JWT with sub, iss, aud, iat, exp, type, and session.
--
-- adminId   - the admin's UUID (as Text)
-- sessionId - the session's UUID (as Text)
-- jwtSecret - the JWT secret from Config
-- domain    - used as both iss and aud
createSignedToken :: TokenType -> Text -> Text -> Text -> Text -> IO (Either JWTError Text)
createSignedToken tokenType adminId sessionId jwtSecret domain = runExceptT $ do
  now <- liftIO getCurrentTime
  let key         = mkSigningKey jwtSecret
      expiry      = addUTCTime (tokenLifetime tokenType) now
      domainClaim = preview stringOrUri domain
      subClaim    = preview stringOrUri adminId
      baseClaims  = TokenClaims emptyClaimsSet (tokenTypeText tokenType) sessionId
      claims =
        baseClaims
          & claimSub .~ subClaim
          & claimIss .~ domainClaim
          & claimAud .~ (Audience . pure <$> domainClaim)
          & claimIat .~ Just (NumericDate now)
          & claimExp .~ Just (NumericDate expiry)
  alg <- bestJWSAlg key
  jwt <- signJWT key (newJWSHeader ((), alg)) claims
  pure (TE.decodeUtf8 (BSL.toStrict (encodeCompact jwt)))

-- Verify a signed JWT.
--
-- tokenType - the expected token type ("access" or "refresh")
-- tokenText - the JWT
-- jwtSecret - the JWT secret from Config
-- domain    - used as both iss and aud
--
-- Checks: signature validity, exp/iat (handled by verifyJWT),
-- iss == domain, aud contains domain, and the typed "type" field
-- matches tokenType (so a refresh token can't be replayed as an
-- access token, or vice versa).
--
-- Returns (admin_id, session_id) from the sub claim and the typed
-- session field.
verifyToken :: TokenType -> Text -> Text -> Text -> IO (Either String (Text, Text))
verifyToken tokenType tokenText jwtSecret domain = do
  result <- runExceptT $ do
    let key = mkSigningKey jwtSecret
    expectedIssuer <- maybe (throwError "invalid domain configured") pure
                         (preview stringOrUri domain)
    jwt <- runJOSE "malformed token" $
      decodeCompact (BSL.fromStrict (TE.encodeUtf8 tokenText))
    claims <- runJOSE "signature or claim validation failed" $
      verifyJWT
        (defaultJWTValidationSettings (== expectedIssuer)
           & issuerPredicate .~ (== expectedIssuer))
        key
        jwt
    subj <- maybe (throwError "missing sub claim") pure (claims ^. claimSub)
    if tcType claims /= tokenTypeText tokenType
      then throwError "incorrect token type"
      else pure (toText subj, tcSession claims)
  pure result
  where
    toText = review stringOrUri

-- exposed functions to create JWT of both types and verify them

createAccessToken :: Text -> Text -> Text -> Text -> IO (Either JWTError Text)
createAccessToken = createSignedToken AccessType

createRefreshToken :: Text -> Text -> Text -> Text -> IO (Either JWTError Text)
createRefreshToken = createSignedToken RefreshType

verifyAccessToken :: Text -> Text -> Text -> IO (Either String (Text, Text))
verifyAccessToken = verifyToken AccessType

verifyRefreshToken :: Text -> Text -> Text -> IO (Either String (Text, Text))
verifyRefreshToken = verifyToken RefreshType

-- necessary definitions to create an auth handler in the API

type AuthResult = (Text, Text)

adminAuthHandler :: AuthHandler Request AuthResult
adminAuthHandler = mkAuthHandler handler
  where
    handler :: Request -> Handler (Text, Text)
    handler req =
      case lookup "Authorization" (requestHeaders req) of
          Nothing ->
              throwError err401

          Just header ->
              case BS.stripPrefix "Bearer " header of
                  Nothing ->
                      throwError err401

                  Just token -> do
                      result <- liftIO $
                          verifyAccessToken (TE.decodeUtf8 token) "" ""

                      case result of
                          Left _ ->
                              throwError err401

                          Right authResult ->
                              pure authResult

authContext :: Context '[AuthHandler Request AuthResult]
authContext =
    adminAuthHandler :. EmptyContext