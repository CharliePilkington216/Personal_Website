{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Logger
  ( Logger
  , newLogger
  , logMessage
  , closeLogger
  ) where

import Control.Concurrent      (ThreadId, forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception       (SomeException, catch)
import Data.Text               (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import Data.Time               (getCurrentTime)
import Data.Time.Format        (defaultTimeLocale, formatTime)
import System.IO
  ( BufferMode (LineBuffering)
  , IOMode (AppendMode)
  , hClose
  , hSetBuffering
  , openFile
  )

-- | Internal messages sent down the channel to the worker thread.
data LogCommand
  = LogEntry Text
  | Shutdown

-- | A handle to a running background logger. Constructed with
-- 'newLogger'; only 'logMessage' and 'closeLogger' should touch it.
data Logger = Logger
  { loggerChan   :: Chan LogCommand
  , loggerThread :: ThreadId
  }

-- | Start a logger writing to the given file path. Spawns a single
-- background thread that owns the file handle and drains the channel;
-- the channel is unbounded, so 'logMessage' never blocks the caller
-- waiting on this thread or on file IO.
newLogger :: FilePath -> IO Logger
newLogger path = do
  chan <- newChan
  tid  <- forkIO (worker path chan)
  pure (Logger chan tid)

worker :: FilePath -> Chan LogCommand -> IO ()
worker path chan = do
  handle <- openFile path AppendMode
  hSetBuffering handle LineBuffering
  loop handle
  where
    loop handle = do
      cmd <- readChan chan
      case cmd of
        Shutdown -> hClose handle
        LogEntry txt -> do
          writeLine handle txt `catch` \(_ :: SomeException) -> pure ()
          loop handle

    writeLine handle txt = do
      now <- getCurrentTime
      let stamp = T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" now)
      TIO.hPutStrLn handle ("[" <> stamp <> "] " <> txt)

-- | Enqueue a message to be written to the log file. Returns
-- immediately — the write itself happens asynchronously on the
-- logger's own thread, so calling this from a request handler adds
-- negligible latency.
logMessage :: Logger -> Text -> IO ()
logMessage logger txt = writeChan (loggerChan logger) (LogEntry txt)

-- | Ask the background thread to flush and close the file. Enqueues a
-- shutdown message rather than killing the thread, so any log entries
-- queued ahead of it are written first.
closeLogger :: Logger -> IO ()
closeLogger logger = writeChan (loggerChan logger) Shutdown