{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Email
-- Description : Low-level sending via the Resend HTTP API, no retry/queueing logic.
--
-- This module knows how to send exactly one email given a fully-resolved
-- 'EmailSettings' and 'EmailContent' — nothing more. It never reads Config
-- or the environment (the caller assembles 'EmailSettings' from those in
-- 'Main.hs'), and it never retries. Retry/backoff and anything
-- inquiry-specific lives in 'InquiryEmail', which calls this.
--
-- Uses Resend's HTTP API (<https://resend.com/docs/api-reference/emails/send-email>)
-- rather than talking SMTP directly — a plain JSON POST over HTTPS, no
-- SMTP client, no separate TLS/crypto stack pulled in just for mail
-- (previously via smtp-mail/HaskellNet-SSL, which is also what was
-- dragging in the conflicting cryptonite dependency).
module Email
  ( EmailSettings (..)
  , EmailContent (..)
  , EmailError (..)
  , sendEmail
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
  ( Manager
  , Request (..)
  , RequestBody (RequestBodyLBS)
  , Response
  , httpLbs
  , parseRequest
  , responseBody
  , responseStatus
  )
import Network.HTTP.Types.Header (hAuthorization, hContentType)
import Network.HTTP.Types.Status (statusIsSuccessful)

-- | Resend connection details, assembled by the caller from Config /
-- \".env\". 'settingsManager' is created once at startup (see 'Main.hs',
-- 'Network.HTTP.Client.TLS.newTlsManager') and threaded through, rather
-- than opening a fresh HTTP connection pool on every send.
data EmailSettings = EmailSettings
  { settingsManager     :: Manager
  , settingsApiKey      :: Text
  -- ^ Resend API key (\"re_...\"), sent as a Bearer token.
  , settingsFromAddress :: Text
  -- ^ Must be on a domain verified with Resend. Can be a bare address
  -- (\"noreply\@charliepilkington.uk\") or \"Name \<email\>\" form.
  }

-- | A single email to send: recipient, subject, plain-text body.
data EmailContent = EmailContent
  { emailTo      :: Text
  , emailSubject :: Text
  , emailBody    :: Text
  }

newtype EmailError = ResendSendFailed Text
  deriving (Show, Eq)

resendEndpoint :: String
resendEndpoint = "https://api.resend.com/emails"

-- | Send one email via the Resend API. Synchronous; does not retry and
-- does not throw — failures (network errors, non-2xx responses) come
-- back as 'Left'.
sendEmail :: EmailSettings -> EmailContent -> IO (Either EmailError ())
sendEmail settings content = do
  result <- try (dispatch settings content)
  pure $ case result of
    Left e -> Left (ResendSendFailed (T.pack (show (e :: SomeException))))
    Right response
      | statusIsSuccessful (responseStatus response) -> Right ()
      | otherwise ->
          Left (ResendSendFailed (TE.decodeUtf8 (LBS.toStrict (responseBody response))))

dispatch :: EmailSettings -> EmailContent -> IO (Response LBS.ByteString)
dispatch settings content = do
  initialRequest <- parseRequest resendEndpoint
  let payload =
        encode $
          object
            [ "from" .= settingsFromAddress settings
            , "to" .= [emailTo content]
            , "subject" .= emailSubject content
            , "text" .= emailBody content
            ]
      request =
        initialRequest
          { method = "POST"
          , requestBody = RequestBodyLBS payload
          , requestHeaders =
              [ (hContentType, "application/json")
              , (hAuthorization, TE.encodeUtf8 ("Bearer " <> settingsApiKey settings))
              ]
          }
  httpLbs request (settingsManager settings)