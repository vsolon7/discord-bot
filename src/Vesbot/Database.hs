module Vesbot.Database
  ( DBConnection
  , initDBConnection
  , withDB
  ) where

import Control.Concurrent.MVar( MVar, withMVar, newMVar )
import Database.SQLite.Simple as SQL

type DBConnection = MVar SQL.Connection

withDB :: DBConnection -> (SQL.Connection -> IO a) -> IO a
withDB = withMVar

initDBConnection :: FilePath -> IO DBConnection
initDBConnection databaseFile = do
  c <- SQL.open databaseFile
  threadSafeDBConnection <- newMVar c
  return threadSafeDBConnection
