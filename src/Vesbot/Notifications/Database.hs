{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Notifications.Database
  ( initTable
  , getDueNotifications
  , getNextNotificationTime
  ) where

import Vesbot.Parsing (intToUTC)
import Vesbot.Database.Types (DBConnection, withDB)
import Vesbot.Notifications.Types (NotificationDataInternal, NotificationsEnv(..), nudge)

import Data.Time (UTCTime)
import Data.Int (Int64)
import qualified Database.SQLite.Simple as SQL


initTable :: DBConnection -> IO ()
initTable dbconn = withDB dbconn $
  \conn -> do
    SQL.execute_ conn
      "CREATE TABLE IF NOT EXISTS notifications(\
      \  id               INTEGER PRIMARY KEY,\
      \  uid              TEXT,\
      \  channel_id       TEXT NOT NULL,\
      \  message_content  TEXT NOT NULL,\
      \  reply_id         TEXT,\
      \  due_at           INTEGER NOT NULL,\
      \  type             TEXT,\
      \  notified         INTEGER NOT NULL DEFAULT 0) STRICT"


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


getDueNotifications :: DBConnection -> UTCTime -> IO [NotificationDataInternal]
getDueNotifications dbconn now = withDB dbconn $
  \conn -> do
    ns <- SQL.query conn q [now]
    return ns
  where
    q =
      "UPDATE notifications SET notified = 1 \
      \WHERE notified = 0 AND due_at <= ? \
      \RETURNING (name, channel_id, message_content, message_reply, created_at, due_at)"
