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

portfolioServer :: Server PortfolioAPI
portfolioServer =
       publicProjectsHandler
  :<|> publicTagsHandler
  :<|> adminEndpointsHandler

-- endpoint definitions

publicProjectsHandler :: Handler [Project]
publicProjectsHandler = undefined

publicTagsHandler :: Handler [Tag]
publicTagsHandler = undefined

adminEndpointsHandler :: (Text, Text) -> Server PortfolioAdminEndpointsAPI
adminEndpointsHandler _ = adminProjectsEndpointsHandler
                    :<|> adminTagsEndpointsHandler

adminProjectsEndpointsHandler :: Server PortfolioAdminProjectsAPI
adminProjectsEndpointsHandler = adminProjectsGetHandler
                              :<|> adminProjectsPostHandler
                              :<|> adminProjectsPutHandler
                              :<|> adminProjectsDeleteHandler

adminTagsEndpointsHandler :: Server PortfolioAdminTagsAPI
adminTagsEndpointsHandler = adminTagsGetHandler
                            :<|> adminTagsPostHandler
                            :<|> adminTagsDeleteHandler

adminProjectsGetHandler :: Handler [ProjectWithID]
adminProjectsGetHandler = undefined

adminProjectsPostHandler :: Project -> Handler ProjectWithID
adminProjectsPostHandler = undefined

adminProjectsPutHandler :: Text -> Project -> Handler ProjectWithID
adminProjectsPutHandler = undefined

adminProjectsDeleteHandler :: Text -> Handler NoContent
adminProjectsDeleteHandler = undefined

adminTagsGetHandler :: Handler [TagWithID]
adminTagsGetHandler = undefined

adminTagsPostHandler :: Tag -> Handler TagWithID
adminTagsPostHandler = undefined

adminTagsDeleteHandler :: Text -> Handler NoContent
adminTagsDeleteHandler = undefined