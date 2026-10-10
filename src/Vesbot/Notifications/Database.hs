{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Notifications.Database
  ( initTable
  , saveNotification
  , getDueNotifications
  , getNextNotificationTime
  ) where

import Vesbot.Utils (showT)
import Vesbot.Parsing (intToUTC, utcToInt)
import Vesbot.Database.Types (DBConnection, withDB)
import Vesbot.Notifications.Types (Notification)

import Data.Time (UTCTime, getCurrentTime)
import Data.Int (Int64)
import qualified Database.SQLite.Simple as SQL


initTable :: DBConnection -> IO ()
initTable dbconn = withDB dbconn $
  \conn -> do
    SQL.execute_ conn
      "CREATE TABLE IF NOT EXISTS notifications(\
      \  id              INTEGER PRIMARY KEY,\
      \  channel_id      TEXT NOT NULL,\
      \  message_content TEXT NOT NULL,\
      \  message_reply   TEXT,\
      \  created_at      INTEGER NOT NULL,\
      \  due_at          INTEGER NOT NULL,\
      \  notified        INTEGER NOT NULL DEFAULT 0) STRICT"


saveNotification :: DBConnection -> Notification -> IO ()
saveNotification dbconn n = withDB dbconn $
  \conn -> do
    SQL.execute conn q n
  where
    q = "INSERT INTO notifications (channel_id, message_content, message_reply, created_at, due_at) \
        \VALUES (?, ?, ?, ?, ?)"


getNextNotificationTime :: DBConnection -> IO (Maybe UTCTime)
getNextNotificationTime dbconn = withDB dbconn $
  \conn -> do
    res <- SQL.query_ conn q :: IO [SQL.Only (Maybe Int64)]
    case res of
      [SQL.Only (Just t)] -> return $ Just (intToUTC t)
      _                   -> return Nothing
  where
    q =
      "SELECT MIN(due_at) \
      \FROM notifications \
      \WHERE notified = 0"


getDueNotifications :: DBConnection -> UTCTime -> IO [Notification]
getDueNotifications dbconn now = withDB dbconn $
  \conn -> do
    now <- getCurrentTime
    ns <- SQL.query conn q [now]
    return ns
  where
    q =
      "UPDATE notifications SET notified = 1 \
      \WHERE notified = 0 AND due_at <= ? \
      \RETURNING (channel_id, message_content, message_reply, created_at, due_at)"
