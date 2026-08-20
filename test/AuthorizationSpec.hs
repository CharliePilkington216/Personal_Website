{-# LANGUAGE OverloadedStrings #-}

module AuthorizationSpec (spec) where

import Test.Hspec
import Authorization
    ( AccessToken (..)
    , LogInRequest (..)
    , TokenMinter
    , loginHandler
    , refreshHandler
    , logoutHandler
    )
import Config (Config, loadConfig, authDbConnString)
import Crypto (hashToken)
import Crypto.JWT (JWTError (JWTExpired))
import Data.Pool (createPool, destroyAllResources, withResource)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Database (DB)
import Database.PostgreSQL.Simple
    ( Connection
    , Only (..)
    , begin
    , close
    , connectPostgreSQL
    , execute_
    , query
    , rollback, execute
    )
import Servant (ServerError (..))
import Servant.API.ResponseHeaders (getHeaders, getResponse)
import Servant.Server (runHandler)
import Web.Cookie (SetCookie (..), parseSetCookie, sameSiteStrict)

-- creates a test db conn but needs a pool, the pool only has 1 connection so we can rollback, don't change this
setupDb :: Text -> [Connection -> IO ()] -> IO DB
setupDb connString seedActions = do
    pool <- createPool (connectPostgreSQL (TE.encodeUtf8 connString)) close 1 60 1
    withResource pool $ \conn -> do
        begin conn
        mapM_ ($ conn) seedActions
    pure pool

teardownDb :: DB -> IO ()
teardownDb pool = do
    withResource pool rollback
    destroyAllResources pool

-- to test handlers with a failed db instance
brokenDb :: IO DB
brokenDb = createPool (connectPostgreSQL "postgresql://invalid:invalid@localhost:1/nonexistent") close 1 60 1

-- ---------------------------------------------------------------------
-- fake token minters
-- ---------------------------------------------------------------------

-- Deterministic fake tokens, tagged by label so a test can assert
-- exactly what came out without needing a real signed JWT.
fakeMinter :: Text -> TokenMinter
fakeMinter label adminId sessionId =
    pure (Right (label <> ":" <> adminId <> ":" <> sessionId))

failingMinter :: TokenMinter
failingMinter _ _ = pure (Left JWTExpired)

-- ---------------------------------------------------------------------
-- shared fixture
-- ---------------------------------------------------------------------

seededAdminId :: Text
seededAdminId = "123e4567-e89b-42d3-a456-426614174000"

seedAdmin :: Connection -> IO ()
seedAdmin conn =
    () <$ execute_ conn
        "INSERT INTO admins (admin_id, email, password_hash) VALUES \
        \('123e4567-e89b-42d3-a456-426614174000', 'admin@example.com', \
        \'$2y$14$riUZ1r4Gkl2GIn8Lln6mIuUSj7Re7gx2Wsb5sLraV757/TrQKfMiy')"

spec :: Spec
spec = do
    config <- runIO loadConfig
    loginHandlerSpec config
    refreshHandlerSpec config
    logoutHandlerSpec config

-- =======================================================================
-- loginHandler
-- =======================================================================

loginHandlerSpec :: Config -> Spec
loginHandlerSpec config =
    beforeAll (setupDb (authDbConnString config) [seedAdmin]) $
        afterAll teardownDb $
            describe "loginHandler" $ do

                it "returns an access token and refresh cookie for correct credentials, \
                   \and persists a new session and token" $ \db -> do
                    result <- runHandler (loginHandler db (fakeMinter "access") (fakeMinter "refresh") req)
                    case result of
                        Left err -> expectationFailure ("expected success, got " <> show (errHTTPCode err))
                        Right resp -> do
                            [Only sessionId] <- withResource db $ \conn ->
                                query conn
                                    "SELECT session_id::text FROM sessions WHERE admin_id = ?"
                                    (Only seededAdminId)
                                    :: IO [Only Text]

                            token (getResponse resp) `shouldBe` ("access:" <> seededAdminId <> ":" <> sessionId)

                            let rawCookie = lookup "Set-Cookie" (getHeaders resp)
                            case rawCookie of
                                Nothing -> expectationFailure "no Set-Cookie header on successful login"
                                Just raw -> do
                                    let cookie = parseSetCookie raw
                                    setCookieName cookie `shouldBe` "refresh_token"
                                    setCookieValue cookie `shouldBe` TE.encodeUtf8 ("refresh:" <> seededAdminId <> ":" <> sessionId)
                                    setCookieHttpOnly cookie `shouldBe` True
                                    setCookieSecure cookie `shouldBe` True
                                    setCookieSameSite cookie `shouldBe` Just sameSiteStrict

                            [(revoked, tokenExists)] <- withResource db $ \conn ->
                                query conn
                                    "SELECT s.revoked, EXISTS (SELECT 1 FROM tokens t WHERE t.session_id = s.session_id AND t.token_hash = ?) \
                                    \FROM sessions s WHERE s.session_id = ?"
                                    (hashToken ("refresh:" <> seededAdminId <> ":" <> sessionId), sessionId)
                                    :: IO [(Bool, Bool)]
                            revoked `shouldBe` False
                            tokenExists `shouldBe` True

                it "returns 401 for an email that isn't registered" $ \db -> do
                    result <- runHandler (loginHandler db (fakeMinter "access") (fakeMinter "refresh")
                        (LogInRequest { email = "nobody@example.com", password = "password" }))
                    case result of
                        Right _  -> expectationFailure "expected 401, got a success"
                        Left err -> errHTTPCode err `shouldBe` 401

                it "returns 401 for the wrong password" $ \db -> do
                    result <- runHandler (loginHandler db (fakeMinter "access") (fakeMinter "refresh")
                        (LogInRequest { email = "admin@example.com", password = "not-the-password" }))
                    case result of
                        Right _  -> expectationFailure "expected 401, got a success"
                        Left err -> errHTTPCode err `shouldBe` 401

                it "returns 500 when the access token minter fails" $ \db -> do
                    result <- runHandler (loginHandler db failingMinter (fakeMinter "refresh") req)
                    case result of
                        Right _  -> expectationFailure "expected 500, got a success"
                        Left err -> errHTTPCode err `shouldBe` 500

                it "returns 500 when the refresh token minter fails" $ \db -> do
                    result <- runHandler (loginHandler db (fakeMinter "access") failingMinter req)
                    case result of
                        Right _  -> expectationFailure "expected 500, got a success"
                        Left err -> errHTTPCode err `shouldBe` 500

                it "returns 500 when both minters fail" $ \db -> do
                    result <- runHandler (loginHandler db failingMinter failingMinter req)
                    case result of
                        Right _  -> expectationFailure "expected 500, got a success"
                        Left err -> errHTTPCode err `shouldBe` 500

                it "returns 500 when the database is unreachable" $ \_db -> do
                    broken <- brokenDb
                    result <- runHandler (loginHandler broken (fakeMinter "access") (fakeMinter "refresh") req)
                    case result of
                        Right _  -> expectationFailure "expected 500, got a success"
                        Left err -> errHTTPCode err `shouldBe` 500
  where
    req = LogInRequest { email = "admin@example.com", password = "password" }

-- =======================================================================
-- refreshHandler
-- =======================================================================

refreshHandlerSpec :: Config -> Spec
refreshHandlerSpec config =
    beforeAll (setupDb (authDbConnString config) [seedAdmin, seedSessions, seedTokens]) $
        afterAll teardownDb $
            describe "refreshHandler" $ do

                it "rotates the token and returns a new access/refresh pair for an active session" $ \db -> do
                    result <- runHandler (refreshHandler db (fakeMinter "access") (fakeMinter "refresh")
                        ("active-token", seededAdminId, activeSessionId))
                    case result of
                        Left err -> expectationFailure ("expected success, got " <> show (errHTTPCode err))
                        Right resp -> do
                            token (getResponse resp) `shouldBe` ("access:" <> seededAdminId <> ":" <> activeSessionId)
                            [(oldRevoked, sessionRevoked, newTokenExists)] <- withResource db $ \conn ->
                                query conn
                                    "SELECT \
                                    \  (SELECT revoked FROM tokens WHERE token_hash = ?), \
                                    \  (SELECT revoked FROM sessions WHERE session_id = ?), \
                                    \  EXISTS (SELECT 1 FROM tokens WHERE token_hash = ?)"
                                    ( hashToken "active-token"
                                    , activeSessionId
                                    , hashToken ("refresh:" <> seededAdminId <> ":" <> activeSessionId)
                                    )
                                    :: IO [(Bool, Bool, Bool)]
                            oldRevoked `shouldBe` True
                            sessionRevoked `shouldBe` False
                            newTokenExists `shouldBe` True

                it "returns 401 for a session_id that doesn't exist" $ \db -> do
                    result <- runHandler (refreshHandler db (fakeMinter "access") (fakeMinter "refresh")
                        ("whatever", seededAdminId, "00000000-0000-0000-0000-000000000000"))
                    case result of
                        Right _  -> expectationFailure "expected 401, got a success"
                        Left err -> errHTTPCode err `shouldBe` 401

                it "returns 401 for a revoked session" $ \db -> do
                    result <- runHandler (refreshHandler db (fakeMinter "access") (fakeMinter "refresh")
                        ("whatever", seededAdminId, revokedSessionId))
                    case result of
                        Right _  -> expectationFailure "expected 401, got a success"
                        Left err -> errHTTPCode err `shouldBe` 401

                it "returns 401 for an expired session" $ \db -> do
                    result <- runHandler (refreshHandler db (fakeMinter "access") (fakeMinter "refresh")
                        ("whatever", seededAdminId, expiredSessionId))
                    case result of
                        Right _  -> expectationFailure "expected 401, got a success"
                        Left err -> errHTTPCode err `shouldBe` 401

                it "returns 401 when the admin_id doesn't match the session's owner" $ \db -> do
                    result <- runHandler (refreshHandler db (fakeMinter "access") (fakeMinter "refresh")
                        ("whatever", "00000000-0000-0000-0000-000000000000", adminMismatchSessionId))
                    case result of
                        Right _  -> expectationFailure "expected 401, got a success"
                        Left err -> errHTTPCode err `shouldBe` 401

                it "returns 401 for a refresh token that isn't recognised for the session, \
                   \and doesn't touch the session" $ \db -> do
                    result <- runHandler (refreshHandler db (fakeMinter "access") (fakeMinter "refresh")
                        ("never-issued-token", seededAdminId, unrecognisedTokenSessionId))
                    case result of
                        Right _  -> expectationFailure "expected 401, got a success"
                        Left err -> errHTTPCode err `shouldBe` 401

                    [Only revoked] <- withResource db $ \conn ->
                        query conn "SELECT revoked FROM sessions WHERE session_id = ?" (Only unrecognisedTokenSessionId)
                            :: IO [Only Bool]
                    revoked `shouldBe` False

                it "returns 401 and revokes the WHOLE session when an already-rotated-out \
                   \token is replayed (reuse detection)" $ \db -> do
                    result <- runHandler (refreshHandler db (fakeMinter "access") (fakeMinter "refresh")
                        ("replayed-token", seededAdminId, replaySessionId))
                    case result of
                        Right _  -> expectationFailure "expected 401, got a success"
                        Left err -> errHTTPCode err `shouldBe` 401

                    [Only revoked] <- withResource db $ \conn ->
                        query conn "SELECT revoked FROM sessions WHERE session_id = ?" (Only replaySessionId)
                            :: IO [Only Bool]
                    revoked `shouldBe` True

                it "returns 500 when a minter fails after validation succeeds, having \
                   \already burned the presented token" $ \db -> do
                    result <- runHandler (refreshHandler db failingMinter failingMinter
                        ("mint-failure-token", seededAdminId, mintFailureSessionId))
                    case result of
                        Right _  -> expectationFailure "expected 500, got a success"
                        Left err -> errHTTPCode err `shouldBe` 500
                    [Only revoked] <- withResource db $ \conn ->
                        query conn "SELECT revoked FROM tokens WHERE token_hash = ?" (Only (hashToken "mint-failure-token"))
                            :: IO [Only Bool]
                    revoked `shouldBe` True

                it "returns 500 when the database is unreachable" $ \_db -> do
                    broken <- brokenDb
                    result <- runHandler (refreshHandler broken (fakeMinter "access") (fakeMinter "refresh")
                        ("whatever", seededAdminId, activeSessionId))
                    case result of
                        Right _  -> expectationFailure "expected 500, got a success"
                        Left err -> errHTTPCode err `shouldBe` 500
    where
        activeSessionId             = "123e4567-e89b-42d3-a456-426614174101" :: Text
        revokedSessionId             = "123e4567-e89b-42d3-a456-426614174102" :: Text
        expiredSessionId             = "123e4567-e89b-42d3-a456-426614174103" :: Text
        adminMismatchSessionId       = "123e4567-e89b-42d3-a456-426614174104" :: Text
        unrecognisedTokenSessionId   = "123e4567-e89b-42d3-a456-426614174105" :: Text
        replaySessionId              = "123e4567-e89b-42d3-a456-426614174106" :: Text
        mintFailureSessionId         = "123e4567-e89b-42d3-a456-426614174107" :: Text

        insertActiveSession :: Connection -> Text -> Bool -> IO ()
        insertActiveSession conn sessionId revoked =
            () <$ execute conn
                "INSERT INTO sessions (session_id, admin_id, revoked, expires_at, created_at) \
                \VALUES (?, ?, ?, now() + interval '30 days', now())"
                (sessionId, seededAdminId, revoked)

        seedSessions :: Connection -> IO ()
        seedSessions conn = do
            mapM_ (\sid -> insertActiveSession conn sid False)
                [activeSessionId, adminMismatchSessionId, unrecognisedTokenSessionId, replaySessionId, mintFailureSessionId]
            insertActiveSession conn revokedSessionId True
            () <$ execute conn
                "INSERT INTO sessions (session_id, admin_id, revoked, expires_at, created_at) \
                \VALUES (?, ?, false, '2000-12-25T00:00:00+00:00', '2000-11-26T00:00:00+00:00')"
                (expiredSessionId, seededAdminId)

        seedTokens :: Connection -> IO ()
        seedTokens conn = do
            _ <- execute conn
                "INSERT INTO tokens (token_hash, session_id, revoked) VALUES (?, ?, false)"
                (hashToken "active-token", activeSessionId)
            _ <- execute conn
                "INSERT INTO tokens (token_hash, session_id, revoked) VALUES (?, ?, true)"
                (hashToken "replayed-token", replaySessionId)
            () <$ execute conn
                "INSERT INTO tokens (token_hash, session_id, revoked) VALUES (?, ?, false)"
                (hashToken "mint-failure-token", mintFailureSessionId)

-- =======================================================================
-- logoutHandler
-- =======================================================================

logoutHandlerSpec :: Config -> Spec
logoutHandlerSpec config =
    beforeAll (setupDb (authDbConnString config) [seedAdmin, seedSessions]) $
        afterAll teardownDb $
            describe "logoutHandler" $ do

                it "revokes the session and clears the refresh cookie" $ \db -> do
                    result <- runHandler (logoutHandler db ("whatever", seededAdminId, activeSessionId))
                    case result of
                        Left err -> expectationFailure ("expected success, got " <> show (errHTTPCode err))
                        Right resp -> do
                            let rawCookie = lookup "Set-Cookie" (getHeaders resp)
                            case rawCookie of
                                Nothing -> expectationFailure "no Set-Cookie header on logout"
                                Just raw -> do
                                    let cookie = parseSetCookie raw
                                    setCookieName cookie `shouldBe` "refresh_token"
                                    setCookieValue cookie `shouldBe` ""
                                    setCookieMaxAge cookie `shouldBe` Just 0

                    [Only revoked] <- withResource db $ \conn ->
                        query conn "SELECT revoked FROM sessions WHERE session_id = ?" (Only activeSessionId)
                            :: IO [Only Bool]
                    revoked `shouldBe` True

                it "still succeeds (no-op) when the session_id doesn't exist -- current \
                   \behaviour: only a DB exception produces a 500, a zero-row UPDATE doesn't" $ \db -> do
                    result <- runHandler (logoutHandler db ("whatever", seededAdminId, "00000000-0000-0000-0000-000000000000"))
                    case result of
                        Left err -> expectationFailure ("expected success, got " <> show (errHTTPCode err))
                        Right _  -> pure ()

                it "does NOT revoke the session when admin_id doesn't match session_id's owner" $ \db -> do
                    result <- runHandler (logoutHandler db ("whatever", "00000000-0000-0000-0000-000000000000", adminMismatchSessionId))
                    case result of
                        Left err -> expectationFailure ("expected success, got " <> show (errHTTPCode err))
                        Right _  -> pure ()

                    [Only revoked] <- withResource db $ \conn ->
                        query conn "SELECT revoked FROM sessions WHERE session_id = ?" (Only adminMismatchSessionId)
                            :: IO [Only Bool]
                    revoked `shouldBe` False

                it "returns 500 when the database is unreachable" $ \_db -> do
                    broken <- brokenDb
                    result <- runHandler (logoutHandler broken ("whatever", seededAdminId, activeSessionId))
                    case result of
                        Right _  -> expectationFailure "expected 500, got a success"
                        Left err -> errHTTPCode err `shouldBe` 500
  where
    activeSessionId       = "123e4567-e89b-42d3-a456-426614174201" :: Text
    adminMismatchSessionId = "123e4567-e89b-42d3-a456-426614174202" :: Text

    seedSessions :: Connection -> IO ()
    seedSessions conn =
        mapM_ (\sid -> () <$ execute conn
            "INSERT INTO sessions (session_id, admin_id, revoked, expires_at, created_at) \
            \VALUES (?, ?, false, now() + interval '30 days', now())"
            (sid, seededAdminId))
            [activeSessionId, adminMismatchSessionId]