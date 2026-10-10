{-# LANGUAGE OverloadedStrings #-}
module Vesbot.ArgumentTimer.Database where

import Vesbot.Logging as Logging (echo)
import Vesbot.Database.Types (DBConnection, withDB)
import Vesbot.Parsing (intToUTC, utcToInt)

import Database.SQLite.Simple as SQL
import Data.Time (UTCTime)
import Data.Int (Int64)


initTable :: DBConnection -> IO ()
initTable dbconn = withDB dbconn $
  \conn -> do
    SQL.execute_ conn
      "CREATE TABLE IF NOT EXISTS argument_timer(\
      \  id      INTEGER PRIMARY KEY,\
      \  time    INTEGER NOT NULL) STRICT"


getLastArgumentTime :: DBConnection -> IO (Maybe UTCTime)
getLastArgumentTime dbconn = do
  res <- lookup
  return (fmap intToUTC res)
  where
    q = "SELECT time FROM argument_timer \
        \WHERE id = (SELECT MAX(id) FROM argument_timer)"
    lookup = withDB dbconn $
      \conn -> do
        res <- SQL.query_ conn q :: IO [SQL.Only Int64]
        case res of
          []    -> return Nothing
          (SQL.Only t:_) -> return (Just t)


updateArgumentTime :: DBConnection -> UTCTime -> IO ()
updateArgumentTime dbconn time = withDB dbconn $
  \conn -> do
    SQL.execute conn q [utcToInt time]
  where
    q = "INSERT INTO argument_timer (time) \
        \VALUES (?)"
