{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- module to organise the /portfolio section of the API
module Portfolio
    ( PortfolioAPI
    , portfolioServer
    , Project (..)
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
    ) where

import Database (DB)
import Data.Aeson (ToJSON(..), FromJSON(..), genericToJSON, genericParseJSON, defaultOptions, camelTo2, Options (fieldLabelModifier))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Servant
import Servant.Server.Experimental.Auth
import Database.PostgreSQL.Simple
import GHC.Int
import Control.Exception
import Database.PostgreSQL.Simple.Types
import Data.Pool
import Control.Monad.IO.Class

-- JSON definitions for the Portfolio API

data Project = Project
  { title       :: Text
  , projectLink :: Text
  , description :: Text
  , tags        :: [Text]
  , projectDate :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON Project where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }
instance FromJSON Project where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

data Tag = Tag
  { name    :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON Tag where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }
instance FromJSON Tag where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

data ProjectWithID = ProjectWithID
  { projectId   :: Text
  , project     :: Project
  } deriving (Show, Eq, Generic)

instance ToJSON ProjectWithID where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

data TagWithID = TagWithID
  { tagId   :: Text
  , tag     :: Tag
  } deriving (Show, Eq, Generic)

instance ToJSON TagWithID where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

-- API type definition

type instance AuthServerData (AuthProtect "admin-auth") = (Text, Text)

type PortfolioAPI = "projects" :> Get '[JSON] [Project]
            :<|> "tags" :> Get '[JSON] [Tag]
            :<|> "admin" :> PortfolioAdminAPI

type PortfolioAdminAPI = AuthProtect "admin-auth" :> PortfolioAdminEndpointsAPI

type PortfolioAdminEndpointsAPI = "projects" :> PortfolioAdminProjectsAPI
                    :<|> "tags" :> PortfolioAdminTagsAPI

type PortfolioAdminProjectsAPI = Get '[JSON] [ProjectWithID]
                :<|> ReqBody '[JSON] Project :> Post '[JSON] ProjectWithID
                :<|> Capture "projectId" Text :> ReqBody '[JSON] Project :> Put '[JSON] ProjectWithID
                :<|> Capture "projectId" Text :> DeleteNoContent

type PortfolioAdminTagsAPI = Get '[JSON] [TagWithID]
                :<|> ReqBody '[JSON] Tag :> Post '[JSON] TagWithID
                :<|> Capture "tagId" Text :> DeleteNoContent

-- portfolio server definition

portfolioServer :: DB -> Server PortfolioAPI
portfolioServer db =
       publicProjectsHandler db
  :<|> publicTagsHandler db
  :<|> adminEndpointsHandler db

adminEndpointsHandler :: DB -> (Text, Text) -> Server PortfolioAdminEndpointsAPI
adminEndpointsHandler db _ = adminProjectsEndpointsHandler db
                    :<|> adminTagsEndpointsHandler db

adminProjectsEndpointsHandler :: DB -> Server PortfolioAdminProjectsAPI
adminProjectsEndpointsHandler db = adminProjectsGetHandler db
                              :<|> adminProjectsPostHandler db
                              :<|> adminProjectsPutHandler db
                              :<|> adminProjectsDeleteHandler db

adminTagsEndpointsHandler :: DB -> Server PortfolioAdminTagsAPI
adminTagsEndpointsHandler db = adminTagsGetHandler db
                            :<|> adminTagsPostHandler db
                            :<|> adminTagsDeleteHandler db

-- endpoint definitions
 
-- selects all the portfolio projects and puts their tags into an array
-- public endpoint, doesn't return UUIDs
publicProjectsHandler :: DB -> Servant.Handler [Project]
publicProjectsHandler db = do
    result <- liftIO (try (withResource db fetchProjects) :: IO (Either SomeException [(Text, Text, Text, UTCTime, PGArray Text)]))
    case result of
        Left _     -> throwError err500
        Right rows -> pure (map toProject rows)
  where
    fetchProjects :: Connection -> IO [(Text, Text, Text, UTCTime, PGArray Text)]
    fetchProjects conn = query_ conn
        "SELECT p.title, p.project_link, p.description, p.project_date, \
        \COALESCE(array_agg(t.name) FILTER (WHERE t.name IS NOT NULL), '{}') \
        \FROM projects p \
        \LEFT JOIN project_tag_link ptl ON ptl.project_id = p.project_id \
        \LEFT JOIN tags t ON t.tag_id = ptl.tag_id \
        \GROUP BY p.project_id \
        \ORDER BY p.project_id"
 
    toProject :: (Text, Text, Text, UTCTime, PGArray Text) -> Project
    toProject (title', projectLink', description', projectDate', PGArray tags') = Project
        { title       = title'
        , projectLink = projectLink'
        , description = description'
        , tags        = tags'
        , projectDate = projectDate'
        }
 
-- selects all tags from the database
-- public endpoint, doesn't return UUIDs
publicTagsHandler :: DB -> Servant.Handler [Tag]
publicTagsHandler db = do
    result <- liftIO (try (withResource db fetchTags) :: IO (Either SomeException [Only Text]))
    case result of
        Left _     -> throwError err500
        Right rows -> pure (map (\(Only n) -> Tag n) rows)
  where
    fetchTags :: Connection -> IO [Only Text]
    fetchTags conn = query_ conn "SELECT name FROM tags ORDER BY name"
 
-- selects all the portfolio projects and puts their tags into an array
-- authorised endpoint, does return UUIDs
adminProjectsGetHandler :: DB -> Servant.Handler [ProjectWithID]
adminProjectsGetHandler db = do
    result <- liftIO (try (withResource db fetchProjects) :: IO (Either SomeException [(Text, Text, Text, Text, UTCTime, PGArray Text)]))
    case result of
        Left _     -> throwError err500
        Right rows -> pure (map toProjectWithID rows)
  where
    fetchProjects :: Connection -> IO [(Text, Text, Text, Text, UTCTime, PGArray Text)]
    fetchProjects conn = query_ conn
        "SELECT p.project_id::text, p.title, p.project_link, p.description, p.project_date, \
        \COALESCE(array_agg(t.name) FILTER (WHERE t.name IS NOT NULL), '{}') \
        \FROM projects p \
        \LEFT JOIN project_tag_link ptl ON ptl.project_id = p.project_id \
        \LEFT JOIN tags t ON t.tag_id = ptl.tag_id \
        \GROUP BY p.project_id \
        \ORDER BY p.project_id"
 
    toProjectWithID :: (Text, Text, Text, Text, UTCTime, PGArray Text) -> ProjectWithID
    toProjectWithID (projectId', title', projectLink', description', projectDate', PGArray tags') = ProjectWithID
        { projectId = projectId'
        , project   = Project
            { title       = title'
            , projectLink = projectLink'
            , description = description'
            , tags        = tags'
            , projectDate = projectDate'
            }
        }
 
-- inserts a new project and links it to each of its tags
-- throws 400 if any given tag name does not exist in the database
-- on success returns the given project along with its newly assigned id
adminProjectsPostHandler :: DB -> Project -> Servant.Handler ProjectWithID
adminProjectsPostHandler db proj = do
    result <- liftIO (try (withResource db (insertProjectWithTags proj)) :: IO (Either SomeException (Maybe Text)))
    case result of
        Left _            -> throwError err500
        Right Nothing     -> throwError err400
        Right (Just pid)  -> pure ProjectWithID { projectId = pid, project = proj }
  where
    insertProjectWithTags :: Project -> Connection -> IO (Maybe Text)
    insertProjectWithTags p conn = withTransaction conn $ do
        maybeTagIds <- lookupTagIds conn (tags p)
        case maybeTagIds of
            Nothing     -> pure Nothing
            Just tagIds -> do
                [Only pid] <- query conn
                    "INSERT INTO projects (title, project_link, description, project_date) \
                    \VALUES (?, ?, ?, ?) RETURNING project_id::text"
                    (title p, projectLink p, description p, projectDate p)
                mapM_ (\tid -> execute conn
                    "INSERT INTO project_tag_link (project_id, tag_id) VALUES (?, ?)"
                    (pid :: Text, tid)) tagIds
                pure (Just pid)
 
-- updates the project matching the given id with the fields of the given project
-- and replaces its tag links with the given tag names
-- throws 400 if the project id does not exist, or if any given tag name does not exist
-- on success returns the given project along with the given id
adminProjectsPutHandler :: DB -> Text -> Project -> Servant.Handler ProjectWithID
adminProjectsPutHandler db pid proj = do
    result <- liftIO (try (withResource db (updateProjectWithTags pid proj)) :: IO (Either SomeException Bool))
    case result of
        Left _      -> throwError err500
        Right False -> throwError err400
        Right True  -> pure ProjectWithID { projectId = pid, project = proj }
  where
    updateProjectWithTags :: Text -> Project -> Connection -> IO Bool
    updateProjectWithTags pid' p conn = withTransaction conn $ do
        maybeTagIds <- lookupTagIds conn (tags p)
        case maybeTagIds of
            Nothing     -> pure False
            Just tagIds -> do
                updated <- execute conn
                    "UPDATE projects SET title = ?, project_link = ?, description = ?, project_date = ? \
                    \WHERE project_id = ?"
                    (title p, projectLink p, description p, projectDate p, pid')
                if updated == (0 :: Int64)
                    then pure False
                    else do
                        _ <- execute conn "DELETE FROM project_tag_link WHERE project_id = ?" (Only pid')
                        mapM_ (\tid -> execute conn
                            "INSERT INTO project_tag_link (project_id, tag_id) VALUES (?, ?)"
                            (pid', tid)) tagIds
                        pure True
 
-- deletes the project matching the given id
-- (its project_tag_link rows are removed automatically via ON DELETE CASCADE)
-- throws 400 if the project id does not exist
-- throws 500 on any other database failure
adminProjectsDeleteHandler :: DB -> Text -> Servant.Handler NoContent
adminProjectsDeleteHandler db pid = do
    result <- liftIO (try (withResource db (deleteProject pid)) :: IO (Either SomeException Int64))
    case result of
        Left _  -> throwError err500
        Right 0 -> throwError err400
        Right _ -> pure NoContent
  where
    deleteProject :: Text -> Connection -> IO Int64
    deleteProject pid' conn = execute conn "DELETE FROM projects WHERE project_id = ?" (Only pid')
 
-- returns every tag including its id
-- throws 500 on any database failure
adminTagsGetHandler :: DB -> Servant.Handler [TagWithID]
adminTagsGetHandler db = do
    result <- liftIO (try (withResource db fetchTags) :: IO (Either SomeException [(Text, Text)]))
    case result of
        Left _     -> throwError err500
        Right rows -> pure (map (\(tid, n) -> TagWithID { tagId = tid, tag = Tag n }) rows)
  where
    fetchTags :: Connection -> IO [(Text, Text)]
    fetchTags conn = query_ conn "SELECT tag_id::text, name FROM tags ORDER BY name"
 
-- inserts a new tag
-- throws 400 if a tag with that name already exists
-- throws 500 on any other database failure
-- on success returns the given tag along with its newly assigned id
adminTagsPostHandler :: DB -> Tag -> Servant.Handler TagWithID
adminTagsPostHandler db t = do
    result <- liftIO (try (withResource db (insertTag t)) :: IO (Either SomeException (Maybe Text)))
    case result of
        Left _           -> throwError err500
        Right Nothing    -> throwError err400
        Right (Just tid) -> pure TagWithID { tagId = tid, tag = t }
  where
    insertTag :: Tag -> Connection -> IO (Maybe Text)
    insertTag t' conn = withTransaction conn $ do
        existing <- query conn "SELECT 1 FROM tags WHERE name = ?" (Only (name t')) :: IO [Only Int]
        case existing of
            (_ : _) -> pure Nothing
            []      -> do
                [Only tid] <- query conn
                    "INSERT INTO tags (name) VALUES (?) RETURNING tag_id::text"
                    (Only (name t'))
                pure (Just tid)
 
-- deletes the tag matching the given id
-- (its project_tag_link rows are removed automatically via ON DELETE CASCADE)
-- throws 400 if the tag id does not exist
-- throws 500 on any other database failure
adminTagsDeleteHandler :: DB -> Text -> Servant.Handler NoContent
adminTagsDeleteHandler db tid = do
    result <- liftIO (try (withResource db (deleteTag tid)) :: IO (Either SomeException Int64))
    case result of
        Left _  -> throwError err500
        Right 0 -> throwError err400
        Right _ -> pure NoContent
  where
    deleteTag :: Text -> Connection -> IO Int64
    deleteTag tid' conn = execute conn "DELETE FROM tags WHERE tag_id = ?" (Only tid')
 
-- helper functions
 
-- looks up the tag_id for each given tag name
-- returns Nothing if any of the given tag names do not exist in the database
lookupTagIds :: Connection -> [Text] -> IO (Maybe [Text])
lookupTagIds conn tagNames = do
    results <- mapM lookupTagId tagNames
    pure (sequence results)
  where
    lookupTagId :: Text -> IO (Maybe Text)
    lookupTagId n = do
        rows <- query conn "SELECT tag_id::text FROM tags WHERE name = ?" (Only n) :: IO [Only Text]
        case rows of
            (Only tid : _) -> pure (Just tid)
            []             -> pure Nothing