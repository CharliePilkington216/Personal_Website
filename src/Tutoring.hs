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
module Tutoring (TutoringAPI, tutoringServer) where

import Database (DB)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant
import Data.Aeson.Types
import Email
import InquiryEmail
import Control.Monad.IO.Class

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

-- tutoring server definition

tutoringServer :: DB -> EmailSettings -> Text -> Server TutoringAPI
tutoringServer db = tutoringInquiriesHandler db

-- endpoint definitions

tutoringInquiriesHandler :: DB -> EmailSettings -> Text -> TutoringRequest -> Handler NoContent
tutoringInquiriesHandler db settings notifyTo req = do
  let details =
        InquiryDetails
          { detailsName     = name req
          , detailsEmail    = email req
          , detailsCategory = categoryLabel (tutoringCategory req)
          , detailsInfo     = description req
          }
 
  liftIO (recordAndNotifyInquiry db settings notifyTo details)
 
  pure NoContent

-- helper functions 

categoryLabel :: TutoringCategory -> Text
categoryLabel Gcse     = "GCSE"
categoryLabel ALevel   = "ALevel"
categoryLabel Oxbridge = "Oxbridge"
categoryLabel Other    = "other"