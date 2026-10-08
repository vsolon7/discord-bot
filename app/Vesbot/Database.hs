module Vesbot.Database where

import Control.Concurrent.MVar
import qualified Database.SQLite.Simple as SQL


type DbConnection = MVar SQL.Connection

withDb :: DbConnection -> (SQL.Connection -> IO a) -> IO a
withDb conn = withMVar conn
