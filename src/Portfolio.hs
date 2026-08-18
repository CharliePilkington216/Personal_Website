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
module Portfolio (PortfolioAPI, portfolioServer) where

import Database (DB)
import Data.Aeson (ToJSON(..), FromJSON(..), genericToJSON, genericParseJSON, defaultOptions, camelTo2, Options (fieldLabelModifier))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Servant
import Servant.Server.Experimental.Auth

-- JSON definitions for the Portfolio API

data Project = Project
  { title       :: Text
  , projectLink :: Text
  , description :: Text
  , tags        :: [Text]
  , projectDate :: UTCTime
  } deriving (Show, Generic)

instance ToJSON Project where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }
instance FromJSON Project where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

data Tag = Tag
  { name    :: Text
  } deriving (Show, Generic)

instance ToJSON Tag where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }
instance FromJSON Tag where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

data ProjectWithID = ProjectWithID
  { projectId   :: Text
  , project     :: Project
  } deriving (Show, Generic)

instance ToJSON ProjectWithID where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

data TagWithID = TagWithID
  { tagId   :: Text
  , tag     :: Tag
  } deriving (Show, Generic)

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
portfolioServer =
       publicProjectsHandler
  :<|> publicTagsHandler
  :<|> adminEndpointsHandler

adminEndpointsHandler :: DB -> (Text, Text) -> Server PortfolioAdminEndpointsAPI
adminEndpointsHandler db _ = adminProjectsEndpointsHandler
                    :<|> adminTagsEndpointsHandler

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

publicProjectsHandler :: DB -> Handler [Project]
publicProjectsHandler db = undefined

publicTagsHandler :: DB -> Handler [Tag]
publicTagsHandler db = undefined

adminProjectsGetHandler :: DB -> Handler [ProjectWithID]
adminProjectsGetHandler db = undefined

adminProjectsPostHandler :: DB -> Project -> Handler ProjectWithID
adminProjectsPostHandler db = undefined

adminProjectsPutHandler :: DB -> Text -> Project -> Handler ProjectWithID
adminProjectsPutHandler db = undefined

adminProjectsDeleteHandler :: DB -> Text -> Handler NoContent
adminProjectsDeleteHandler db = undefined

adminTagsGetHandler :: DB -> Handler [TagWithID]
adminTagsGetHandler db = undefined

adminTagsPostHandler :: DB -> Tag -> Handler TagWithID
adminTagsPostHandler db = undefined

adminTagsDeleteHandler :: DB -> Text -> Handler NoContent
adminTagsDeleteHandler db = undefined