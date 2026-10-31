{-# LANGUAGE OverloadedStrings #-}
module Vesbot.ArgumentTimer.Database where

import Vesbot.Utils (showT)
import Vesbot.Database.Types (DBConnection, withDB)
import Vesbot.Parsing (intToUTC, utcToInt)

import Discord.Types

import Database.SQLite.Simple as SQL
import qualified Data.Text as T
import Data.Int (Int64)


initTable :: DBConnection -> IO ()
initTable dbconn = withDB dbconn $
  \conn -> do
    SQL.execute_ conn
      "CREATE TABLE  IF NOT EXISTS argument_timer(\
      \  id          INTEGER PRIMARY KEY,\
      \  time        INTEGER NOT NULL,\
      \  channel_id  TEXT NOT NULL,\
      \  message_id  TEXT NOT NULL) STRICT"


getLastArgument :: DBConnection -> IO (Maybe (UTCTime, T.Text, T.Text))
getLastArgument dbconn = withDB dbconn $
  \conn -> do
    res <- SQL.query_ conn q :: IO [(Int64, T.Text, T.Text)]
    case res of
      []    -> return Nothing
      ((t, cid, mid):_) -> return (Just (intToUTC t, cid, mid))
  where
    q = "SELECT time, channel_id, message_id FROM argument_timer \
        \WHERE id = (SELECT MAX(id) FROM argument_timer)"


updateArgumentTime :: DBConnection -> UTCTime -> ChannelId -> MessageId -> IO ()
updateArgumentTime dbconn time cid mid = withDB dbconn $
  \conn -> do
    SQL.execute conn q (utcToInt time, showT cid, showT mid)
  where
    q = "INSERT INTO argument_timer (time, channel_id, message_id) \
        \VALUES (?, ?, ?)"
