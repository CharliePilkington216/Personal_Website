{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- module to organise the /tutoring section of the API
module Tutoring where

import Data.Aeson (ToJSON(..), FromJSON(..), genericToJSON, genericParseJSON, defaultOptions, camelTo2, Options (fieldLabelModifier))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Servant

-- JSON definitions for the Tutoring API

data TutoringCategory = Gcse | ALevel | Oxbridge | Other deriving (Show, Generic)

instance FromJSON TutoringCategory where
  parseJSON = withText "tutoring_category" $ \t -> case t of
    "gcse"     -> pure Gcse
    "alevel"   -> pure ALevel
    "oxbridge" -> pure Oxbridge
    "other"    -> pure Other
    _          -> fail $ "Invalid tutoring category: " ++ show t

data TutoringRequest = TutoringRequest
  { name                :: Text
  , email               :: Text
  , tutoringCategory    :: TutoringCategory
  , description         :: Text
  } deriving (Show, Generic)

instance FromJSON TutoringRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

-- API type definition

type TutoringAPI = "inquiries" :> ReqBody '[JSON] TutoringRequest :> Post '[JSON] NoContent