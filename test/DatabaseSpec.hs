{-# LANGUAGE OverloadedStrings #-}

module DatabaseSpec (spec) where

import Test.Hspec
import Database.PostgreSQL.Simple
    ( Connection
    , connectPostgreSQL
    , close
    , execute_
    , query_
    , begin
    , rollback, execute, query
    )
import Database.PostgreSQL.Simple.Types (Only(..))
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

import Config (Config, loadConfig, authDbConnString, portfolioDbConnString, inquiryDbConnString)
import Crypto (hashToken)

-- | Open a raw connection to the given database, start a transaction,
-- and run the supplied seed statements inside it. Everything inserted
-- here only ever exists within this transaction, and disappears when
-- 'teardownConn' rolls it back once the describe block finishes.
setupConn :: Text -> [Connection -> IO ()] -> IO Connection
setupConn connString seedActions = do
    conn <- connectPostgreSQL (TE.encodeUtf8 connString)
    begin conn
    mapM_ ($ conn) seedActions
    pure conn

-- | Roll back everything 'setupConn' inserted and close the
-- connection, leaving the test database exactly as it was.
teardownConn :: Connection -> IO ()
teardownConn conn = do
    rollback conn
    close conn

spec :: Spec
spec = do
    config <- runIO loadConfig
    authDbSpec config
    portfolioDbSpec config
    inquiryDbSpec config

-- ---------------------------------------------------------------------
-- authdb
-- ---------------------------------------------------------------------
authDbSpec :: Config -> Spec
authDbSpec config =
    beforeAll (setupConn (authDbConnString config) [seedAdmins, seedSessions, seedTokens]) $
        afterAll teardownConn $
            describe "authdb: queries used by the /auth endpoints" $ do

                describe "login: SELECT admin_id::text, password_hash FROM admins WHERE email = ?" $ do
                    it "finds the seeded admin by email" $ \conn -> do
                        rows <- query conn
                            "SELECT admin_id::text, password_hash FROM admins WHERE email = ?"
                            (Only ("admin@example.com" :: Text))
                            :: IO [(Text, Text)]
                        rows `shouldBe`
                            [ ( "123e4567-e89b-42d3-a456-426614174000"
                              , "$2y$14$riUZ1r4Gkl2GIn8Lln6mIuUSj7Re7gx2Wsb5sLraV757/TrQKfMiy"
                              )
                            ]

                    it "returns nothing for an email that isn't registered" $ \conn -> do
                        rows <- query conn
                            "SELECT admin_id::text, password_hash FROM admins WHERE email = ?"
                            (Only ("nobody@example.com" :: Text))
                            :: IO [(Text, Text)]
                        rows `shouldBe` []

                describe "login/refresh: INSERT INTO sessions (admin_id) VALUES (?) RETURNING session_id::text" $
                    it "creates a new, non-revoked session that expires ~30 days out" $ \conn -> do
                        [Only newSessionId] <- query conn
                            "INSERT INTO sessions (admin_id) VALUES (?) RETURNING session_id::text"
                            (Only ("123e4567-e89b-42d3-a456-426614174000" :: Text))
                            :: IO [Only Text]

                        [(revoked, withinWindow)] <- query conn
                            "SELECT revoked, expires_at BETWEEN now() + interval '29 days' AND now() + interval '31 days' \
                            \FROM sessions WHERE session_id = ?"
                            (Only newSessionId)
                            :: IO [(Bool, Bool)]

                        revoked `shouldBe` False
                        withinWindow `shouldBe` True

                describe "login/refresh: INSERT INTO tokens (token_hash, session_id) VALUES (?, ?)" $
                    it "stores a token as non-revoked, tied to the given session" $ \conn -> do
                        let tokenHash = hashToken "insert-test-token"

                        _ <- execute conn
                            "INSERT INTO tokens (token_hash, session_id) VALUES (?, ?)"
                            (tokenHash, "123e4567-e89b-42d3-a456-426614174003" :: Text)

                        rows <- query conn
                            "SELECT session_id::text, revoked, created_at IS NOT NULL FROM tokens WHERE token_hash = ?"
                            (Only tokenHash)
                            :: IO [(Text, Bool, Bool)]

                        rows `shouldBe` [("123e4567-e89b-42d3-a456-426614174003", False, True)]

                describe "refresh: SELECT 1 FROM sessions WHERE session_id = ? AND admin_id = ? AND revoked = false AND expires_at > now()" $ do
                    it "matches an active, non-expired, non-revoked session for the correct admin" $ \conn -> do
                        rows <- query conn
                            "SELECT 1 FROM sessions WHERE session_id = ? AND admin_id = ? AND revoked = false AND expires_at > now()"
                            ("123e4567-e89b-42d3-a456-426614174003" :: Text, "123e4567-e89b-42d3-a456-426614174000" :: Text)
                            :: IO [Only Int]
                        rows `shouldBe` [Only 1]

                    it "rejects an expired session" $ \conn -> do
                        rows <- query conn
                            "SELECT 1 FROM sessions WHERE session_id = ? AND admin_id = ? AND revoked = false AND expires_at > now()"
                            ("123e4567-e89b-42d3-a456-426614174001" :: Text, "123e4567-e89b-42d3-a456-426614174000" :: Text)
                            :: IO [Only Int]
                        rows `shouldBe` []

                    it "rejects a revoked session" $ \conn -> do
                        rows <- query conn
                            "SELECT 1 FROM sessions WHERE session_id = ? AND admin_id = ? AND revoked = false AND expires_at > now()"
                            ("123e4567-e89b-42d3-a456-426614174002" :: Text, "123e4567-e89b-42d3-a456-426614174000" :: Text)
                            :: IO [Only Int]
                        rows `shouldBe` []

                    it "rejects a session that doesn't belong to the given admin" $ \conn -> do
                        rows <- query conn
                            "SELECT 1 FROM sessions WHERE session_id = ? AND admin_id = ? AND revoked = false AND expires_at > now()"
                            ("123e4567-e89b-42d3-a456-426614174003" :: Text, "00000000-0000-0000-0000-000000000000" :: Text)
                            :: IO [Only Int]
                        rows `shouldBe` []

                describe "refresh: SELECT revoked FROM tokens WHERE token_hash = ? AND session_id = ?" $ do
                    it "finds an active token as not revoked" $ \conn -> do
                        rows <- query conn
                            "SELECT revoked FROM tokens WHERE token_hash = ? AND session_id = ?"
                            (hashToken "active-token-1", "123e4567-e89b-42d3-a456-426614174003" :: Text)
                            :: IO [Only Bool]
                        rows `shouldBe` [Only False]

                    it "finds an already-rotated-out token as revoked" $ \conn -> do
                        rows <- query conn
                            "SELECT revoked FROM tokens WHERE token_hash = ? AND session_id = ?"
                            (hashToken "revoked-token-1", "123e4567-e89b-42d3-a456-426614174003" :: Text)
                            :: IO [Only Bool]
                        rows `shouldBe` [Only True]

                    it "finds nothing for an unrecognised token" $ \conn -> do
                        rows <- query conn
                            "SELECT revoked FROM tokens WHERE token_hash = ? AND session_id = ?"
                            (hashToken "never-issued-token", "123e4567-e89b-42d3-a456-426614174003" :: Text)
                            :: IO [Only Bool]
                        rows `shouldBe` []

                describe "refresh: UPDATE sessions SET revoked = true WHERE session_id = ? (reuse detection)" $
                    it "revokes the session when a rotated-out token is replayed" $ \conn -> do
                        _ <- execute conn
                            "UPDATE sessions SET revoked = true WHERE session_id = ?"
                            (Only ("123e4567-e89b-42d3-a456-426614174004" :: Text))

                        [Only revoked] <- query conn
                            "SELECT revoked FROM sessions WHERE session_id = ?"
                            (Only ("123e4567-e89b-42d3-a456-426614174004" :: Text))
                            :: IO [Only Bool]
                        revoked `shouldBe` True

                describe "refresh: UPDATE tokens SET revoked = true WHERE token_hash = ? (rotation)" $
                    it "revokes a token once it's been used to refresh" $ \conn -> do
                        let tokenHash = hashToken "rotation-target-token"
                        _ <- execute conn
                            "UPDATE tokens SET revoked = true WHERE token_hash = ?"
                            (Only tokenHash)

                        [Only revoked] <- query conn
                            "SELECT revoked FROM tokens WHERE token_hash = ?"
                            (Only tokenHash)
                            :: IO [Only Bool]
                        revoked `shouldBe` True

                describe "logout: UPDATE sessions SET revoked = true WHERE session_id = ? AND admin_id = ?" $ do
                    it "revokes the session when session_id and admin_id both match" $ \conn -> do
                        _ <- execute conn
                            "UPDATE sessions SET revoked = true WHERE session_id = ? AND admin_id = ?"
                            ("123e4567-e89b-42d3-a456-426614174005" :: Text, "123e4567-e89b-42d3-a456-426614174000" :: Text)

                        [Only revoked] <- query conn
                            "SELECT revoked FROM sessions WHERE session_id = ?"
                            (Only ("123e4567-e89b-42d3-a456-426614174005" :: Text))
                            :: IO [Only Bool]
                        revoked `shouldBe` True

                    it "leaves the session untouched when admin_id doesn't match" $ \conn -> do
                        _ <- execute conn
                            "UPDATE sessions SET revoked = true WHERE session_id = ? AND admin_id = ?"
                            ("123e4567-e89b-42d3-a456-426614174006" :: Text, "00000000-0000-0000-0000-000000000000" :: Text)

                        [Only revoked] <- query conn
                            "SELECT revoked FROM sessions WHERE session_id = ?"
                            (Only ("123e4567-e89b-42d3-a456-426614174006" :: Text))
                            :: IO [Only Bool]
                        revoked `shouldBe` False
  where
    seedAdmins conn =
        () <$ execute_ conn
            "INSERT INTO admins (admin_id, email, password_hash) VALUES \
            \('123e4567-e89b-42d3-a456-426614174000', 'admin@example.com', \
            \'$2y$14$riUZ1r4Gkl2GIn8Lln6mIuUSj7Re7gx2Wsb5sLraV757/TrQKfMiy')"

    seedSessions conn = do
        -- 174001: not revoked, but expired -- isolates the
        -- "expires_at > now()" half of the refresh session check.
        _ <- execute_ conn
            "INSERT INTO sessions (session_id, admin_id, revoked, expires_at, created_at) VALUES \
            \('123e4567-e89b-42d3-a456-426614174001', '123e4567-e89b-42d3-a456-426614174000', \
            \false, '2000-12-25T00:00:00+00:00', '2000-11-26T00:00:00+00:00')"

        -- 174002: revoked, but NOT expired -- isolates the
        -- "revoked = false" half of the same check. (Deliberately
        -- different from the old seed, which had this session both
        -- revoked and at/past its own expiry -- that made it
        -- impossible to tell which clause was actually rejecting it.)
        _ <- execute_ conn
            "INSERT INTO sessions (session_id, admin_id, revoked, expires_at, created_at) VALUES \
            \('123e4567-e89b-42d3-a456-426614174002', '123e4567-e89b-42d3-a456-426614174000', \
            \true, now() + interval '30 days', now())"

        -- 174003: active session, used only by read-only tests
        -- (session-check happy path, token lookups). Never mutated,
        -- so its state stays predictable no matter the test order.
        _ <- execute_ conn
            "INSERT INTO sessions (session_id, admin_id, revoked, expires_at, created_at) VALUES \
            \('123e4567-e89b-42d3-a456-426614174003', '123e4567-e89b-42d3-a456-426614174000', \
            \false, now() + interval '30 days', now())"

        -- 174004/174005/174006: one dedicated active session per
        -- UPDATE test below, kept separate from 174003 and each
        -- other so a mutation in one test can't leak into another.
        _ <- execute_ conn
            "INSERT INTO sessions (session_id, admin_id, revoked, expires_at, created_at) VALUES \
            \('123e4567-e89b-42d3-a456-426614174004', '123e4567-e89b-42d3-a456-426614174000', \
            \false, now() + interval '30 days', now())"
        _ <- execute_ conn
            "INSERT INTO sessions (session_id, admin_id, revoked, expires_at, created_at) VALUES \
            \('123e4567-e89b-42d3-a456-426614174005', '123e4567-e89b-42d3-a456-426614174000', \
            \false, now() + interval '30 days', now())"
        () <$ execute_ conn
            "INSERT INTO sessions (session_id, admin_id, revoked, expires_at, created_at) VALUES \
            \('123e4567-e89b-42d3-a456-426614174006', '123e4567-e89b-42d3-a456-426614174000', \
            \false, now() + interval '30 days', now())"

    seedTokens conn = do
        -- Tied to the active session 174003: one live, one already
        -- rotated out -- for the token-lookup read-only tests.
        _ <- execute conn
            "INSERT INTO tokens (token_hash, session_id, revoked) VALUES (?, ?, false)"
            (hashToken "active-token-1", "123e4567-e89b-42d3-a456-426614174003" :: Text)
        _ <- execute conn
            "INSERT INTO tokens (token_hash, session_id, revoked) VALUES (?, ?, true)"
            (hashToken "revoked-token-1", "123e4567-e89b-42d3-a456-426614174003" :: Text)

        -- Dedicated target for the "revoke on rotation" UPDATE test.
        () <$ execute conn
            "INSERT INTO tokens (token_hash, session_id, revoked) VALUES (?, ?, false)"
            (hashToken "rotation-target-token", "123e4567-e89b-42d3-a456-426614174003" :: Text)

-- ---------------------------------------------------------------------
-- portfoliodb
-- ---------------------------------------------------------------------
portfolioDbSpec :: Config -> Spec
portfolioDbSpec config =
    beforeAll (setupConn (portfolioDbConnString config) [seedProjects, seedTags, seedLinks]) $
        afterAll teardownConn $
            describe "portfoliodb" $ do

                it "connects and sees the seeded project" $ \conn -> do
                    [Only n] <- query_ conn "SELECT count(*) FROM projects" :: IO [Only Int]
                    n `shouldBe` 1

                it "connects and sees all seeded tags" $ \conn -> do
                    [Only n] <- query_ conn "SELECT count(*) FROM tags" :: IO [Only Int]
                    n `shouldBe` 3

                it "has the project linked to all three tags" $ \conn -> do
                    [Only n] <- query_ conn "SELECT count(*) FROM project_tag_link" :: IO [Only Int]
                    n `shouldBe` 3

                it "can join projects to tags through the link table" $ \conn -> do
                    rows <- query_ conn
                        "SELECT t.name FROM tags t \
                        \JOIN project_tag_link l ON l.tag_id = t.tag_id \
                        \JOIN projects p ON p.project_id = l.project_id \
                        \WHERE p.title = 'Personal Website' \
                        \ORDER BY t.name"
                        :: IO [Only String]
                    map (\(Only x) -> x) rows `shouldBe` ["Haskell", "Postgresql", "REST APIs"]
  where
    seedProjects conn =
        () <$ execute_ conn
            "INSERT INTO projects (project_id, title, project_link, description, project_date, created_at) VALUES \
            \('123e4567-e89b-42d3-a456-426614174000', 'Personal Website', \
            \'github.com/CharliePilkington216/Personal_Website', \
            \'This is the website you are currently on!', \
            \'2026-08-19T00:00:00+00:00', '2026-08-19T00:00:00+00:00')"

    seedTags conn =
        () <$ execute_ conn
            "INSERT INTO tags (tag_id, name) VALUES \
            \('123e4567-e89b-42d3-a456-426614174001', 'Haskell'), \
            \('123e4567-e89b-42d3-a456-426614174002', 'REST APIs'), \
            \('123e4567-e89b-42d3-a456-426614174003', 'Postgresql')"

    seedLinks conn =
        () <$ execute_ conn
            "INSERT INTO project_tag_link (project_id, tag_id) VALUES \
            \('123e4567-e89b-42d3-a456-426614174000', '123e4567-e89b-42d3-a456-426614174001'), \
            \('123e4567-e89b-42d3-a456-426614174000', '123e4567-e89b-42d3-a456-426614174002'), \
            \('123e4567-e89b-42d3-a456-426614174000', '123e4567-e89b-42d3-a456-426614174003')"

-- ---------------------------------------------------------------------
-- inquirydb (no seed data, per the schema doc)
-- ---------------------------------------------------------------------
inquiryDbSpec :: Config -> Spec
inquiryDbSpec config =
    beforeAll (setupConn (inquiryDbConnString config) []) $
        afterAll teardownConn $
            describe "inquirydb" $ do

                it "connects and starts with no inquiries" $ \conn -> do
                    [Only n] <- query_ conn "SELECT count(*) FROM inquiries" :: IO [Only Int]
                    n `shouldBe` 0

                it "can insert and then see a new inquiry within this transaction" $ \conn -> do
                    _ <- execute_ conn
                        "INSERT INTO inquiries (name, email, tutoring, info, status) VALUES \
                        \('Test Student', 'student@example.com', 'GCSE', \
                        \'Looking for help with GCSE maths', 'pending')"
                    [Only n] <- query_ conn "SELECT count(*) FROM inquiries" :: IO [Only Int]
                    n `shouldBe` 1