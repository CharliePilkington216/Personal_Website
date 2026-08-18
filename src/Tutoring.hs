{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Tutoring where

import Data.Aeson (ToJSON(..), FromJSON(..), genericToJSON, genericParseJSON, defaultOptions, camelTo2, Options (fieldLabelModifier))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Servant

-- JSON definitions for the Tutoring API

data TutoringCategory = Gcse | ALevel | Oxbridge | Other deriving (Show, Generic)

data TutoringRequest = TutoringRequest
  { name                :: Text
  , email               :: Text
  , tutoringCategory    :: TutoringCategory
  , description         :: Text
  } deriving (Show, Generic)

-- API type definition

type TutoringAPI = "inquiries" :> ReqBody '[JSON] TutoringRequest :> Post '[JSON] NoContent