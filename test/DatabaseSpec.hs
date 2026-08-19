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
    , rollback
    , query
    )
import Database.PostgreSQL.Simple.Types (Only(..))
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

import Config (Config, loadConfig, authDbConnString, portfolioDbConnString, inquiryDbConnString)

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
    beforeAll (setupConn (authDbConnString config) [seedAdmins, seedSessions]) $
        afterAll teardownConn $
            describe "authdb" $ do
                it "connects and sees the seeded admin" $ \conn -> do
                    [Only n] <- query_ conn "SELECT count(*) FROM admins" :: IO [Only Int]
                    n `shouldBe` 1

                it "connects and sees both seeded sessions" $ \conn -> do
                    [Only n] <- query_ conn "SELECT count(*) FROM sessions" :: IO [Only Int]
                    n `shouldBe` 2

                it "can look up the seeded admin by email" $ \conn -> do
                    [Only email] <- query_ conn
                        "SELECT email FROM admins WHERE admin_id = '123e4567-e89b-42d3-a456-426614174000'"
                        :: IO [Only String]
                    email `shouldBe` "admin@example.com"

                it "can distinguish the revoked session from the active one" $ \conn -> do
                    [Only n] <- query_ conn
                        "SELECT count(*) FROM sessions WHERE revoked = true"
                        :: IO [Only Int]
                    n `shouldBe` 1
  where
    seedAdmins conn =
        () <$ execute_ conn
            "INSERT INTO admins (admin_id, email, password_hash) VALUES \
            \('123e4567-e89b-42d3-a456-426614174000', 'admin@example.com', \
            \'$2y$14$riUZ1r4Gkl2GIn8Lln6mIuUSj7Re7gx2Wsb5sLraV757/TrQKfMiy')"

    seedSessions conn = do
        _ <- execute_ conn
            "INSERT INTO sessions (session_id, admin_id, revoked, expires_at, created_at) VALUES \
            \('123e4567-e89b-42d3-a456-426614174001', '123e4567-e89b-42d3-a456-426614174000', \
            \false, '2000-12-25T00:00:00+00:00', '2000-11-26T00:00:00+00:00')"
        () <$ execute_ conn
            "INSERT INTO sessions (session_id, admin_id, revoked, expires_at, created_at) VALUES \
            \('123e4567-e89b-42d3-a456-426614174002', '123e4567-e89b-42d3-a456-426614174000', \
            \true, now(), now() + interval '30 days')"

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