{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : InquiryEmail
-- Description : Persists a tutoring inquiry and notifies the site owner in the background.
--
-- Matches the @inquiries@ table in @inquirydb@ exactly as documented in
-- @Documentation/DATABASE_SCHEMA.md@ (columns @name@, @email@, @tutoring@,
-- @info@, @status@; enums @tutoring_type@ and @inquiry_status@) — no
-- migration needed, the table already exists.
--
-- 'recordDeniedInquiry' is kept here and exported even though nothing
-- currently calls it automatically (email-deliverability checking was
-- removed) — it's the natural hook for a future blacklist feature: reject
-- and record as 'denied' without ever attempting to notify.
--
-- Flow, called from 'Tutoring.tutoringInquiriesHandler':
--
--   * 'recordAndNotifyInquiry' inserts the row as @'pending'@ and forks a
--     background process ('notifyLoop') that emails the site owner via
--     Resend (see 'Email'), retrying with exponential backoff (1s, 2s,
--     4s, ... capped at 2^15s) on failure. Returns as soon as the insert
--     completes — the Servant handler never waits on the background send.
--   * Once backoff is exhausted the row is marked @'failed'@ and an
--     apology email goes to the inquirer directly; if that also fails,
--     nothing further happens (left for a future logging pass).
--
-- The retry process is in-process only (a forked thread, not a
-- DB-persisted job queue): if the server restarts mid-backoff, that
-- inquiry's row is left @'pending'@ and won't be retried further.
module InquiryEmail
  ( InquiryDetails (..)
  , recordDeniedInquiry
  , recordAndNotifyInquiry
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple

import Email (EmailContent (..), EmailError (..), EmailSettings, sendEmail)
import Logger

-- | The fields needed to record and notify about an inquiry. 'detailsCategory'
-- must already be the exact Postgres @tutoring_type@ enum label ("GCSE",
-- "ALevel", "Oxbridge", or "other") — 'Tutoring' converts its
-- 'Tutoring.TutoringCategory' to this before calling in, so this module
-- doesn't need to import Tutoring's types (which would create an import
-- cycle, since Tutoring calls into this module).
data InquiryDetails = InquiryDetails
  { detailsName     :: Text
  , detailsEmail    :: Text
  , detailsCategory :: Text
  , detailsInfo     :: Text
  }

-- | Record an inquiry as 'denied'. Synchronous, one-shot: no notification
-- is attempted and nothing runs in the background. Not currently called
-- from the inquiry handler — available for a future blacklist check.
recordDeniedInquiry :: Pool Connection -> InquiryDetails -> IO ()
recordDeniedInquiry pool details =
  withResource pool $ \conn ->
    () <$
      execute
        conn
        "INSERT INTO inquiries (name, email, tutoring, info, status) \
        \VALUES (?, ?, ?::tutoring_type, ?, 'denied'::inquiry_status)"
        (detailsName details, detailsEmail details, detailsCategory details, detailsInfo details)

-- | Record an inquiry as 'pending', then fork a background process that
-- tries to email the site owner about it. Returns as soon as the row is
-- inserted.
recordAndNotifyInquiry :: Logger -> Pool Connection -> EmailSettings -> Text -> InquiryDetails -> IO ()
recordAndNotifyInquiry logger pool settings notifyTo details = do
  inquiryId <- withResource pool $ \conn -> do
    [Only iid] <-
      query
        conn
        "INSERT INTO inquiries (name, email, tutoring, info, status) \
        \VALUES (?, ?, ?::tutoring_type, ?, 'pending'::inquiry_status) \
        \RETURNING inquiry_id::text"
        (detailsName details, detailsEmail details, detailsCategory details, detailsInfo details)
    pure iid
  _ <- forkIO (notifyLoop logger pool settings notifyTo inquiryId details 0)
  pure ()

-- | 2^15 seconds — the backoff value at which we stop retrying and give
-- up rather than schedule another wait.
maxBackoffSeconds :: Int
maxBackoffSeconds = 2 ^ (15 :: Int)

-- | The background retry loop. 'attempt' counts failed attempts so far
-- (0 on the very first try, not yet attempted when this is called). On
-- failure it waits @2 ^ attempt@ seconds — 1s, 2s, 4s, ... — before
-- retrying, giving up once that delay would reach 'maxBackoffSeconds'.
notifyLoop :: Logger -> Pool Connection -> EmailSettings -> Text -> Text -> InquiryDetails -> Int -> IO ()
notifyLoop logger pool settings notifyTo inquiryId details attempt = do
  result <- sendEmail settings (notificationContent notifyTo details)
  case result of
    Right () -> do
      logMessage logger "Email sent due to request on /tutoring/inquiries"
      markStatus pool inquiryId "sent"
    Left (ResendSendFailed _) -> do
      let delaySeconds = (2 :: Int) ^ attempt
      if delaySeconds >= maxBackoffSeconds
        then do
          logMessage logger "Email failed to send due to request on /tutoring/inquiries, failed"
          giveUp logger pool settings inquiryId details
        else do
          logMessage logger "Email failed to send due to request on /tutoring/inquiries, retrying"
          threadDelay (delaySeconds * 1000000)
          notifyLoop logger pool settings notifyTo inquiryId details (attempt + 1)

-- | Backoff exhausted: mark the row 'failed' and try to apologise to the
-- inquirer directly. If the apology also fails, do nothing further —
-- left for a future logging pass rather than retried again.
giveUp :: Logger -> Pool Connection -> EmailSettings -> Text -> InquiryDetails -> IO ()
giveUp logger pool settings inquiryId details = do
  markStatus pool inquiryId "failed"
  apologyResult <- sendEmail settings (apologyContent details)
  case apologyResult of
    Right () -> pure ()
    Left _   -> do
      logMessage logger "Apology email failed to send due to failed request on /tutoring/inquiries"      
      pure ()

markStatus :: Pool Connection -> Text -> Text -> IO ()
markStatus pool inquiryId status =
  withResource pool $ \conn ->
    () <$
      execute
        conn
        "UPDATE inquiries SET status = ?::inquiry_status WHERE inquiry_id = ?::uuid"
        (status, inquiryId)

notificationContent :: Text -> InquiryDetails -> EmailContent
notificationContent notifyTo details =
  EmailContent
    { emailTo = notifyTo
    , emailSubject = "New tutoring inquiry: " <> detailsCategory details
    , emailBody =
        T.unlines
          [ "A new tutoring inquiry has been submitted."
          , ""
          , "Name: " <> detailsName details
          , "Email: " <> detailsEmail details
          , "Category: " <> detailsCategory details
          , ""
          , "Message:"
          , detailsInfo details
          ]
    }

apologyContent :: InquiryDetails -> EmailContent
apologyContent details =
  EmailContent
    { emailTo = detailsEmail details
    , emailSubject = "Sorry - we couldn't process your tutoring inquiry"
    , emailBody =
        T.unlines
          [ "Hi " <> detailsName details <> ","
          , ""
          , "Thanks for reaching out about tutoring. Unfortunately we ran into a "
              <> "technical problem processing your inquiry and it didn't reach us properly."
          , ""
          , "Could you try emailing directly instead? Sorry for the hassle."
          ]
    }