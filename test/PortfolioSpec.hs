{-# LANGUAGE OverloadedStrings #-}

module PortfolioSpec (spec) where

import Test.Hspec
import Portfolio
    ( Project (..)
    , Tag (..)
    , ProjectWithID (..)
    , TagWithID (..)
    , publicProjectsHandler
    , publicTagsHandler
    , adminProjectsGetHandler
    , adminProjectsPostHandler
    , adminProjectsPutHandler
    , adminProjectsDeleteHandler
    , adminTagsGetHandler
    , adminTagsPostHandler
    , adminTagsDeleteHandler
    )
import Config (Config, loadConfig, portfolioDbConnString)
import Data.List (sort)
import Data.Pool (createPool, destroyAllResources, withResource)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Database (DB)
import Database.PostgreSQL.Simple
    ( Connection
    , Only (..)
    , begin
    , close
    , connectPostgreSQL
    , execute_
    , query
    , query_
    , rollback
    )
import Servant (NoContent (..), ServerError (..))
import Servant.Server (runHandler)
import Control.Exception (finally)

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
-- to get around the fact that some tests can't be rolled back easily
-- ---------------------------------------------------------------------

withFreshDb :: Config -> [Connection -> IO ()] -> (DB -> IO a) -> IO a
withFreshDb config seedActions action = do
    db <- setupDbNoTransaction (portfolioDbConnString config) seedActions
    action db `finally` destroyAllResources db

setupDbNoTransaction :: Text -> [Connection -> IO ()] -> IO DB
setupDbNoTransaction connString seedActions = do
    pool <- createPool (connectPostgreSQL (TE.encodeUtf8 connString)) close 1 60 1
    withResource pool $ \conn ->
        mapM_ ($ conn) seedActions
    pure pool

-- ---------------------------------------------------------------------
-- shared fixtures
-- ---------------------------------------------------------------------

testDate :: UTCTime
testDate = UTCTime (fromGregorian 2026 8 19) (secondsToDiffTime 0)

mkProject :: [Text] -> Project
mkProject tags' = Project
    { title       = "Personal Website"
    , projectLink = "github.com/CharliePilkington216/Personal_Website"
    , description = "This is the website you are currently on!"
    , tags        = tags'
    , projectDate = testDate
    }

spec :: Spec
spec = do
    config <- runIO loadConfig
    publicProjectsHandlerSpec config
    publicTagsHandlerSpec config
    adminProjectsGetHandlerSpec config
    --adminProjectsPostHandlerSpec config
    --adminProjectsPutHandlerSpec config
    adminProjectsDeleteHandlerSpec config
    adminTagsGetHandlerSpec config
    --adminTagsPostHandlerSpec config
    adminTagsDeleteHandlerSpec config

-- =======================================================================
-- publicProjectsHandler
-- =======================================================================

publicProjectsHandlerSpec :: Config -> Spec
publicProjectsHandlerSpec config =
    beforeAll (setupDb (portfolioDbConnString config) [seedProjects, seedTags, seedLinks]) $
        afterAll teardownDb $
            describe "publicProjectsHandler" $ do

                it "returns every project without an id, tags sorted alphabetically" $ \db -> do
                    result <- runHandler (publicProjectsHandler db)
                    case result of
                        Left err -> expectationFailure ("expected success, got " <> show (errHTTPCode err))
                        Right projects -> do
                            sort (map title projects) `shouldBe` ["Personal Website", "Tagless Project"]
                            lookupByTitle "Personal Website" projects `shouldBe`
                                Just (mkProject ["Haskell", "Postgresql"])
                            lookupByTitle "Tagless Project" projects `shouldBe`
                                Just (mkProject []) { title = "Tagless Project" }

                it "returns an empty list when there are no projects" $ \_db -> do
                    empty <- setupDb (portfolioDbConnString config) []
                    result <- runHandler (publicProjectsHandler empty)
                    teardownDb empty
                    case result of
                        Left err       -> expectationFailure ("expected success, got " <> show (errHTTPCode err))
                        Right projects -> projects `shouldBe` []

                it "returns 500 when the database is unreachable" $ \_db -> do
                    broken <- brokenDb
                    result <- runHandler (publicProjectsHandler broken)
                    case result of
                        Right _  -> expectationFailure "expected 500, got a success"
                        Left err -> errHTTPCode err `shouldBe` 500
  where
    lookupByTitle :: Text -> [Project] -> Maybe Project
    lookupByTitle t = foldr (\p acc -> if title p == t then Just p else acc) Nothing

-- =======================================================================
-- publicTagsHandler
-- =======================================================================

publicTagsHandlerSpec :: Config -> Spec
publicTagsHandlerSpec config =
    beforeAll (setupDb (portfolioDbConnString config) [seedTags]) $
        afterAll teardownDb $
            describe "publicTagsHandler" $ do

                it "returns every tag, alphabetically" $ \db -> do
                    result <- runHandler (publicTagsHandler db)
                    case result of
                        Left err     -> expectationFailure ("expected success, got " <> show (errHTTPCode err))
                        Right tags'  -> map name tags' `shouldBe` ["Haskell", "Postgresql"]

                it "returns 500 when the database is unreachable" $ \_db -> do
                    broken <- brokenDb
                    result <- runHandler (publicTagsHandler broken)
                    case result of
                        Right _  -> expectationFailure "expected 500, got a success"
                        Left err -> errHTTPCode err `shouldBe` 500

-- =======================================================================
-- adminProjectsGetHandler
-- =======================================================================

adminProjectsGetHandlerSpec :: Config -> Spec
adminProjectsGetHandlerSpec config =
    beforeAll (setupDb (portfolioDbConnString config) [seedProjects, seedTags, seedLinks]) $
        afterAll teardownDb $
            describe "adminProjectsGetHandler" $ do

                it "returns every project with its id and tags" $ \db -> do
                    result <- runHandler (adminProjectsGetHandler db)
                    case result of
                        Left err       -> expectationFailure ("expected success, got " <> show (errHTTPCode err))
                        Right projects -> do
                            lookupById taggedProjectId projects `shouldBe`
                                Just (mkProject ["Haskell", "Postgresql"])
                            lookupById taglessProjectId projects `shouldBe`
                                Just (mkProject []) { title = "Tagless Project" }

                it "returns 500 when the database is unreachable" $ \_db -> do
                    broken <- brokenDb
                    result <- runHandler (adminProjectsGetHandler broken)
                    case result of
                        Right _  -> expectationFailure "expected 500, got a success"
                        Left err -> errHTTPCode err `shouldBe` 500
  where
    lookupById :: Text -> [ProjectWithID] -> Maybe Project
    lookupById pid = foldr (\pw acc -> if projectId pw == pid then Just (project pw) else acc) Nothing

-- =======================================================================
-- adminProjectsPostHandler
-- =======================================================================

adminProjectsPostHandlerSpec :: Config -> Spec
adminProjectsPostHandlerSpec config =
    describe "adminProjectsPostHandler" $ do

        it "inserts a project, links it to each existing tag, and returns it with a new id" $ do
            withFreshDb config [seedTags] $ \db -> do
                let newProject = mkProject ["Haskell", "Postgresql"]

                result <- runHandler (adminProjectsPostHandler db newProject)

                case result of
                    Left err ->
                        expectationFailure
                            ("expected success, got " <> show (errHTTPCode err))

                    Right created -> do
                        project created `shouldBe` newProject

                        [Only n] <- withResource db $ \conn ->
                            query conn
                                "SELECT count(*) FROM projects WHERE project_id = ?"
                                (Only (projectId created))
                                :: IO [Only Int]

                        n `shouldBe` (1 :: Int)

                        linkedTags <- withResource db $ \conn ->
                            query conn
                                "SELECT t.name FROM tags t \
                                \JOIN project_tag_link l ON l.tag_id = t.tag_id \
                                \WHERE l.project_id = ? ORDER BY t.name"
                                (Only (projectId created))
                                :: IO [Only Text]

                        map (\(Only x) -> x) linkedTags
                            `shouldBe` ["Haskell", "Postgresql"]

        it "inserts a project with no tags and no links" $ do
            withFreshDb config [seedTags] $ \db -> do
                result <- runHandler
                    (adminProjectsPostHandler db (mkProject []))

                case result of
                    Left err ->
                        expectationFailure
                            ("expected success, got " <> show (errHTTPCode err))

                    Right created -> do
                        [Only n] <- withResource db $ \conn ->
                            query conn
                                "SELECT count(*) FROM project_tag_link WHERE project_id = ?"
                                (Only (projectId created))
                                :: IO [Only Int]

                        n `shouldBe` (0 :: Int)

        it "returns 400 and inserts nothing when a given tag doesn't exist" $ do
            withFreshDb config [seedTags] $ \db -> do
                [Only before] <- withResource db $ \conn ->
                    query_ conn
                        "SELECT count(*) FROM projects"
                        :: IO [Only Int]

                result <- runHandler
                    (adminProjectsPostHandler db
                        (mkProject ["Haskell", "NotARealTag"]))

                case result of
                    Right _ ->
                        expectationFailure "expected 400, got a success"

                    Left err ->
                        errHTTPCode err `shouldBe` 400

                [Only after] <- withResource db $ \conn ->
                    query_ conn
                        "SELECT count(*) FROM projects"
                        :: IO [Only Int]

                after `shouldBe` (before :: Int)

        it "returns 500 when the database is unreachable" $ do
            broken <- brokenDb

            result <- runHandler
                (adminProjectsPostHandler broken (mkProject []))

            destroyAllResources broken

            case result of
                Right _ ->
                    expectationFailure "expected 500, got a success"

                Left err ->
                    errHTTPCode err `shouldBe` 500

-- =======================================================================
-- adminProjectsPutHandler
-- =======================================================================

adminProjectsPutHandlerSpec :: Config -> Spec
adminProjectsPutHandlerSpec config =
    describe "adminProjectsPutHandler" $ do

        it "updates the project's fields and replaces its tag links" $ do
            withFreshDb config
                [seedPutProjects, seedTags, seedPutLinks] $ \db -> do

                let updated =
                        (mkProject ["Postgresql"])
                            { title = "Updated Title" }

                result <- runHandler
                    (adminProjectsPutHandler db putUpdateTargetId updated)

                case result of
                    Left err ->
                        expectationFailure
                            ("expected success, got " <> show (errHTTPCode err))

                    Right resp -> do
                        projectId resp `shouldBe` putUpdateTargetId
                        project resp `shouldBe` updated

                        [Only storedTitle] <- withResource db $ \conn ->
                            query conn
                                "SELECT title FROM projects WHERE project_id = ?"
                                (Only putUpdateTargetId)
                                :: IO [Only Text]

                        storedTitle `shouldBe` "Updated Title"

                        linkedTags <- withResource db $ \conn ->
                            query conn
                                "SELECT t.name FROM tags t \
                                \JOIN project_tag_link l ON l.tag_id = t.tag_id \
                                \WHERE l.project_id = ? ORDER BY t.name"
                                (Only putUpdateTargetId)
                                :: IO [Only Text]

                        map (\(Only x) -> x) linkedTags
                            `shouldBe` ["Postgresql"]

        it "returns 400 and changes nothing when the project id doesn't exist" $ do
            withFreshDb config
                [seedPutProjects, seedTags, seedPutLinks] $ \db -> do

                result <- runHandler
                    (adminProjectsPutHandler db
                        "00000000-0000-0000-0000-000000000000"
                        (mkProject ["Haskell"]))

                case result of
                    Right _ ->
                        expectationFailure "expected 400, got a success"

                    Left err ->
                        errHTTPCode err `shouldBe` 400

                [Only storedTitle] <- withResource db $ \conn ->
                    query conn
                        "SELECT title FROM projects WHERE project_id = ?"
                        (Only putUntouchedId)
                        :: IO [Only Text]

                storedTitle `shouldBe` "Personal Website"

                linkedTags <- withResource db $ \conn ->
                    query conn
                        "SELECT t.name FROM tags t \
                        \JOIN project_tag_link l ON l.tag_id = t.tag_id \
                        \WHERE l.project_id = ? ORDER BY t.name"
                        (Only putUntouchedId)
                        :: IO [Only Text]

                map (\(Only x) -> x) linkedTags
                    `shouldBe` ["Haskell", "Postgresql"]

        it "returns 400 and leaves the original row/links untouched when a given tag doesn't exist" $ do
            withFreshDb config
                [seedPutProjects, seedTags, seedPutLinks] $ \db -> do

                result <- runHandler
                    (adminProjectsPutHandler db
                        putUntouchedId
                        (mkProject ["NotARealTag"]))

                case result of
                    Right _ ->
                        expectationFailure "expected 400, got a success"

                    Left err ->
                        errHTTPCode err `shouldBe` 400

                [Only storedTitle] <- withResource db $ \conn ->
                    query conn
                        "SELECT title FROM projects WHERE project_id = ?"
                        (Only putUntouchedId)
                        :: IO [Only Text]

                storedTitle `shouldBe` "Personal Website"

                linkedTags <- withResource db $ \conn ->
                    query conn
                        "SELECT t.name FROM tags t \
                        \JOIN project_tag_link l ON l.tag_id = t.tag_id \
                        \WHERE l.project_id = ? ORDER BY t.name"
                        (Only putUntouchedId)
                        :: IO [Only Text]

                map (\(Only x) -> x) linkedTags
                    `shouldBe` ["Haskell", "Postgresql"]

        it "returns 500 when the database is unreachable" $ do
            broken <- brokenDb

            result <- runHandler
                (adminProjectsPutHandler broken putUntouchedId (mkProject []))

            destroyAllResources broken

            case result of
                Right _ ->
                    expectationFailure "expected 500, got a success"

                Left err ->
                    errHTTPCode err `shouldBe` 500

  where
    putUpdateTargetId, putUntouchedId :: Text
    putUpdateTargetId =
        "123e4567-e89b-42d3-a456-426614174320"

    putUntouchedId =
        "123e4567-e89b-42d3-a456-426614174321"

    seedPutProjects :: Connection -> IO ()
    seedPutProjects conn =
        () <$ execute_ conn
            "INSERT INTO projects (project_id, title, project_link, description, project_date, created_at) VALUES \
            \('123e4567-e89b-42d3-a456-426614174320', 'Personal Website', \
            \'github.com/CharliePilkington216/Personal_Website', \
            \'This is the website you are currently on!', \
            \'2026-08-19T00:00:00+00:00', '2026-08-19T00:00:00+00:00'), \
            \('123e4567-e89b-42d3-a456-426614174321', 'Personal Website', \
            \'github.com/CharliePilkington216/Personal_Website', \
            \'This is the website you are currently on!', \
            \'2026-08-19T00:00:00+00:00', '2026-08-19T00:00:00+00:00')"

    seedPutLinks :: Connection -> IO ()
    seedPutLinks conn =
        () <$ execute_ conn
            "INSERT INTO project_tag_link (project_id, tag_id) VALUES \
            \('123e4567-e89b-42d3-a456-426614174320', '123e4567-e89b-42d3-a456-426614174310'), \
            \('123e4567-e89b-42d3-a456-426614174320', '123e4567-e89b-42d3-a456-426614174311'), \
            \('123e4567-e89b-42d3-a456-426614174321', '123e4567-e89b-42d3-a456-426614174310'), \
            \('123e4567-e89b-42d3-a456-426614174321', '123e4567-e89b-42d3-a456-426614174311')"


-- =======================================================================
-- adminProjectsDeleteHandler
-- =======================================================================

adminProjectsDeleteHandlerSpec :: Config -> Spec
adminProjectsDeleteHandlerSpec config =
    beforeAll (setupDb (portfolioDbConnString config) [seedProjects, seedTags, seedLinks]) $
        afterAll teardownDb $
            describe "adminProjectsDeleteHandler" $ do

                it "deletes the project and cascades its tag links, tags themselves untouched" $ \db -> do
                    result <- runHandler (adminProjectsDeleteHandler db taggedProjectId)
                    case result of
                        Left err -> expectationFailure ("expected success, got " <> show (errHTTPCode err))
                        Right nc -> nc `shouldBe` NoContent

                    [Only projectCount] <- withResource db $ \conn ->
                        query conn "SELECT count(*) FROM projects WHERE project_id = ?" (Only taggedProjectId)
                            :: IO [Only Int]
                    projectCount `shouldBe` (0 :: Int)

                    [Only linkCount] <- withResource db $ \conn ->
                        query conn "SELECT count(*) FROM project_tag_link WHERE project_id = ?" (Only taggedProjectId)
                            :: IO [Only Int]
                    linkCount `shouldBe` (0 :: Int)

                    [Only tagCount] <- withResource db $ \conn ->
                        query_ conn "SELECT count(*) FROM tags" :: IO [Only Int]
                    tagCount `shouldBe` (2 :: Int)

                it "returns 400 when the project id doesn't exist" $ \db -> do
                    result <- runHandler (adminProjectsDeleteHandler db "00000000-0000-0000-0000-000000000000")
                    case result of
                        Right _  -> expectationFailure "expected 400, got a success"
                        Left err -> errHTTPCode err `shouldBe` 400

                it "returns 500 when the database is unreachable" $ \_db -> do
                    broken <- brokenDb
                    result <- runHandler (adminProjectsDeleteHandler broken taggedProjectId)
                    case result of
                        Right _  -> expectationFailure "expected 500, got a success"
                        Left err -> errHTTPCode err `shouldBe` 500

-- =======================================================================
-- adminTagsGetHandler
-- =======================================================================

adminTagsGetHandlerSpec :: Config -> Spec
adminTagsGetHandlerSpec config =
    beforeAll (setupDb (portfolioDbConnString config) [seedTags]) $
        afterAll teardownDb $
            describe "adminTagsGetHandler" $ do

                it "returns every tag with its id, alphabetically by name" $ \db -> do
                    result <- runHandler (adminTagsGetHandler db)
                    case result of
                        Left err   -> expectationFailure ("expected success, got " <> show (errHTTPCode err))
                        Right tws  -> map (\tw -> (tagId tw, name (tag tw))) tws `shouldBe`
                            [ (haskellTagId, "Haskell")
                            , (postgresqlTagId, "Postgresql")
                            ]

                it "returns 500 when the database is unreachable" $ \_db -> do
                    broken <- brokenDb
                    result <- runHandler (adminTagsGetHandler broken)
                    case result of
                        Right _  -> expectationFailure "expected 500, got a success"
                        Left err -> errHTTPCode err `shouldBe` 500

-- =======================================================================
-- adminTagsPostHandler
-- =======================================================================

adminTagsPostHandlerSpec :: Config -> Spec
adminTagsPostHandlerSpec config =
    describe "adminTagsPostHandler" $ do

        it "inserts a new tag and returns it with a new id" $ do
            withFreshDb config [seedTags] $ \db -> do
                result <- runHandler
                    (adminTagsPostHandler db (Tag "REST APIs"))

                case result of
                    Left err ->
                        expectationFailure
                            ("expected success, got " <> show (errHTTPCode err))

                    Right created -> do
                        tag created `shouldBe` Tag "REST APIs"

                        [Only n] <- withResource db $ \conn ->
                            query conn
                                "SELECT count(*) FROM tags WHERE tag_id = ? AND name = ?"
                                (tagId created, "REST APIs" :: Text)
                                :: IO [Only Int]

                        n `shouldBe` (1 :: Int)

        it "returns 400 and inserts nothing when the tag name already exists" $ do
            withFreshDb config [seedTags] $ \db -> do
                [Only before] <- withResource db $ \conn ->
                    query_ conn
                        "SELECT count(*) FROM tags"
                        :: IO [Only Int]

                result <- runHandler
                    (adminTagsPostHandler db (Tag "Haskell"))

                case result of
                    Right _ ->
                        expectationFailure "expected 400, got a success"

                    Left err ->
                        errHTTPCode err `shouldBe` 400

                [Only after] <- withResource db $ \conn ->
                    query_ conn
                        "SELECT count(*) FROM tags"
                        :: IO [Only Int]

                after `shouldBe` (before :: Int)

        it "returns 500 when the database is unreachable" $ do
            broken <- brokenDb

            result <- runHandler
                (adminTagsPostHandler broken (Tag "Anything"))

            destroyAllResources broken

            case result of
                Right _ ->
                    expectationFailure "expected 500, got a success"

                Left err ->
                    errHTTPCode err `shouldBe` 500

-- =======================================================================
-- adminTagsDeleteHandler
-- =======================================================================

adminTagsDeleteHandlerSpec :: Config -> Spec
adminTagsDeleteHandlerSpec config =
    beforeAll (setupDb (portfolioDbConnString config) [seedProjects, seedTags, seedLinks]) $
        afterAll teardownDb $
            describe "adminTagsDeleteHandler" $ do

                it "deletes a tag and cascades its links, without deleting the project" $ \db -> do
                    result <- runHandler (adminTagsDeleteHandler db haskellTagId)
                    case result of
                        Left err -> expectationFailure ("expected success, got " <> show (errHTTPCode err))
                        Right nc -> nc `shouldBe` NoContent

                    [Only tagCount] <- withResource db $ \conn ->
                        query conn "SELECT count(*) FROM tags WHERE tag_id = ?" (Only haskellTagId)
                            :: IO [Only Int]
                    tagCount `shouldBe` (0 :: Int)

                    [Only linkCount] <- withResource db $ \conn ->
                        query conn "SELECT count(*) FROM project_tag_link WHERE tag_id = ?" (Only haskellTagId)
                            :: IO [Only Int]
                    linkCount `shouldBe` (0 :: Int)

                    [Only projectCount] <- withResource db $ \conn ->
                        query conn "SELECT count(*) FROM projects WHERE project_id = ?" (Only taggedProjectId)
                            :: IO [Only Int]
                    projectCount `shouldBe` (1 :: Int)

                it "returns 400 when the tag id doesn't exist" $ \db -> do
                    result <- runHandler (adminTagsDeleteHandler db "00000000-0000-0000-0000-000000000000")
                    case result of
                        Right _  -> expectationFailure "expected 400, got a success"
                        Left err -> errHTTPCode err `shouldBe` 400

                it "returns 500 when the database is unreachable" $ \_db -> do
                    broken <- brokenDb
                    result <- runHandler (adminTagsDeleteHandler broken postgresqlTagId)
                    case result of
                        Right _  -> expectationFailure "expected 500, got a success"
                        Left err -> errHTTPCode err `shouldBe` 500

-- ---------------------------------------------------------------------
-- shared seed data
-- ---------------------------------------------------------------------

taggedProjectId, taglessProjectId :: Text
taggedProjectId  = "123e4567-e89b-42d3-a456-426614174300"
taglessProjectId = "123e4567-e89b-42d3-a456-426614174301"

haskellTagId, postgresqlTagId :: Text
haskellTagId    = "123e4567-e89b-42d3-a456-426614174310"
postgresqlTagId = "123e4567-e89b-42d3-a456-426614174311"

-- one project with two tags, and one project with none, so tag-aggregation
-- and the empty-tags edge case are both exercised by the same seed
seedProjects :: Connection -> IO ()
seedProjects conn =
    () <$ execute_ conn
        "INSERT INTO projects (project_id, title, project_link, description, project_date, created_at) VALUES \
        \('123e4567-e89b-42d3-a456-426614174300', 'Personal Website', \
        \'github.com/CharliePilkington216/Personal_Website', \
        \'This is the website you are currently on!', \
        \'2026-08-19T00:00:00+00:00', '2026-08-19T00:00:00+00:00'), \
        \('123e4567-e89b-42d3-a456-426614174301', 'Tagless Project', \
        \'github.com/CharliePilkington216/Personal_Website', \
        \'This is the website you are currently on!', \
        \'2026-08-19T00:00:00+00:00', '2026-08-19T00:00:00+00:00')"

seedTags :: Connection -> IO ()
seedTags conn =
    () <$ execute_ conn
        "INSERT INTO tags (tag_id, name) VALUES \
        \('123e4567-e89b-42d3-a456-426614174310', 'Haskell'), \
        \('123e4567-e89b-42d3-a456-426614174311', 'Postgresql')"

seedLinks :: Connection -> IO ()
seedLinks conn =
    () <$ execute_ conn
        "INSERT INTO project_tag_link (project_id, tag_id) VALUES \
        \('123e4567-e89b-42d3-a456-426614174300', '123e4567-e89b-42d3-a456-426614174310'), \
        \('123e4567-e89b-42d3-a456-426614174300', '123e4567-e89b-42d3-a456-426614174311')"