module Database (createDB, DB) where

import Data.Pool (Pool, createPool)
import Database.PostgreSQL.Simple
    ( Connection
    , connectPostgreSQL
    , close
    )
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

type DB = Pool Connection

createDB :: Text -> IO DB
createDB connString =
    createPool
        (connectPostgreSQL (TE.encodeUtf8 connString))
        close
        1       -- number of stripes
        60      -- unused connection timeout
        10      -- maximum connections per stripe