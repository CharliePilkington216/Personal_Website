{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Portfolio where

import Data.Aeson (ToJSON(..), FromJSON(..), genericToJSON, genericParseJSON, defaultOptions, camelTo2, Options (fieldLabelModifier))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Servant

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

type PortfolioAPI = "projects" :> Get '[JSON] [Project]
            :<|> "tags" :> Get '[JSON] [Tag]
            :<|> "admin" :> (
              "projects" :> (
                Get '[JSON] [ProjectWithID]
                :<|> ReqBody '[JSON] Project :> Post '[JSON] ProjectWithID
                :<|> Capture "projectId" Text :> (
                  ReqBody '[JSON] Project :> Put '[JSON] ProjectWithID
                  :<|> DeleteNoContent
                )
              )
              :<|> "tags" :> (
                Get '[JSON] [TagWithID]
                :<|> ReqBody '[JSON] Tag :> Post '[JSON] TagWithID
                :<|> Capture "tagId" Text :> DeleteNoContent
              )
            )