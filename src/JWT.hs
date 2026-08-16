{-# LANGUAGE OverloadedStrings #-}

-- module to handle the creation and verification of JWTs for authentication
module JWT (createAccessToken, createRefreshToken, verifyAccessToken, verifyRefreshToken) where

import Control.Lens ((&), (.~), (^.), preview, review)
import Control.Monad.Except (runExceptT, throwError)
import Crypto.JOSE.Header (Protection (Protected))
import Crypto.JOSE.JWK (JWK, fromOctets)
import Crypto.JWT
  ( Audience (..)
  , ClaimsSet
  , JWTError
  , NumericDate (..)
  , addClaim
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
  , signClaims
  , stringOrUri
  , unregisteredClaims
  , verifyClaims
  )
import Data.Aeson (Value (String))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Time (NominalDiffTime, addUTCTime, getCurrentTime)

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

-- Build and sign a JWT with sub, iss, aud, iat, exp, and type.
--
-- adminId   - the admin's UUID (as Text)
-- jwtSecret - the JWT secret from Config
-- domain    - used as both iss and aud
createSignedToken :: TokenType -> Text -> Text -> Text -> IO (Either JWTError Text)
createSignedToken tokenType adminId jwtSecret domain = runExceptT $ do
  now <- liftIO getCurrentTime
  let key         = mkSigningKey jwtSecret
      expiry      = addUTCTime (tokenLifetime tokenType) now
      domainClaim = preview stringOrUri domain
      subClaim    = preview stringOrUri adminId
      claims =
        emptyClaimsSet
          & claimSub .~ subClaim
          & claimIss .~ domainClaim
          & claimAud .~ (Audience . pure <$> domainClaim)
          & claimIat .~ Just (NumericDate now)
          & claimExp .~ Just (NumericDate expiry)
          & addClaim "type" (String (tokenTypeText tokenType))
  alg <- bestJWSAlg key
  jwt <- signClaims key (newJWSHeader (Protected, alg)) claims
  pure (TE.decodeUtf8 (BSL.toStrict (encodeCompact jwt)))

-- Verify a signed JWT.
--
-- tokenText    - the JWT
-- jwtSecret - the JWT secret from Config
-- domain    - used as both iss and aud
--
-- Checks: signature validity, exp/iat (handled by verifyClaims),
-- iss == domain, aud contains domain, and the custom "type" claim
-- equals "access" (so a refresh token can't be replayed as an access
-- token).
verifyToken :: TokenType -> Text -> Text -> Text -> IO (Either String Text)
verifyToken tokenType domain jwtSecret tokenText = do
  result <- runExceptT $ do
    let key = mkSigningKey jwtSecret
    expectedIssuer <- maybe (throwError "invalid domain configured") pure
                         (preview stringOrUri domain)
    jwt <- decodeCompact (BSL.fromStrict (TE.encodeUtf8 tokenText))
             `orError` "malformed token"
    claims <- verifyClaims
                (defaultJWTValidationSettings (== expectedIssuer)
                   & issuerPredicate .~ (== expectedIssuer))
                key
                jwt
              `orError` "signature or claim validation failed"
    subj <- maybe (throwError "missing sub claim") pure (claims ^. claimSub)
    let claimedTokenType = HM.lookup "type" (claims ^. unregisteredClaims)
    if claimedTokenType /= Just (String (tokenTypeText tokenType))
      then throwError "incorrect token type"
      else pure (toText subj)
  pure result
  where
    orError action msg = action >>= either (const (throwError msg)) pure
    toText = review stringOrUri

-- exposed functions to create JWT of both types and verify them

createAccessToken :: Text -> Text -> Text -> IO (Either JWTError Text)
createAccessToken = createSignedToken AccessType

createRefreshToken :: Text -> Text -> Text -> IO (Either JWTError Text)
createRefreshToken = createSignedToken RefreshType

verifyAccessToken :: Text -> Text -> Text -> IO (Either String Text)
verifyAccessToken = verifyToken AccessType

verifyRefreshToken :: Text -> Text -> Text -> IO (Either String Text)
verifyRefreshToken = verifyToken RefreshType