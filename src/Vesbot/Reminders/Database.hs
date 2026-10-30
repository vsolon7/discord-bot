{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Reminders.Database where

import Vesbot.Reminders.Types
import Vesbot.Database.Types
import Vesbot.Parsing (intToUTC)

import Discord.Internal.Types

import Data.Int (Int64)
import qualified Database.SQLite.Simple as SQL


initTable :: DBConnection -> IO ()
initTable dbconn = withDB dbconn $ \conn ->
  SQL.execute_ conn
    "CREATE TABLE IF NOT EXISTS reminders(\
    \  id             INTEGER PRIMARY KEY,\
    \  user_id        TEXT NOT NULL,\
    \  content        TEXT NOT NULL,\
    \  created_at     INTEGER NOT NULL,\
    \  due_at         INTEGER NOT NULL,\
    \  notified       INTEGER NOT NULL DEFAULT 0) STRICT"

saveReminder :: DBConnection -> ReminderData -> IO Int64
saveReminder dbconn r = withDB dbconn $
  \conn -> do
    x <- SQL.query conn
      "INSERT INTO reminders \
      \(user_id, content, created_at, due_at) \
      \VALUES (?, ?, ?, ?) \
      \RETURNING id" r
    case x of
      [SQL.Only (Just rid)] -> return rid
      _ -> error "Somehow failed to get primary key after inserting a reminder into the database."

getReminderById :: DBConnection -> Int64 -> IO (Maybe ReminderData)
getReminderById dbconn rid = withDB dbconn $
  \conn -> do
    mr <- SQL.query conn
      "SELECT user_id, content, created_at, due_at \
      \FROM reminders \
      \WHERE id = ?"
      [rid] :: IO [ReminderData]
    case mr of
      (rd:_) -> return (Just rd)
      _      -> return Nothing

nextReminderTime :: DBConnection -> IO (Maybe UTCTime)
nextReminderTime dbconn = withDB dbconn $
  \conn -> do
    nextList <- SQL.query_ conn
                  "SELECT MIN(due_at) \
                  \FROM reminders \
                  \WHERE notified = 0"
    case nextList of
      [SQL.Only (Just t)] -> return $ Just (intToUTC t)
      _                   -> return Nothing

markAsNotified :: DBConnection -> Int64 -> IO ()
markAsNotified dbconn rid = withDB dbconn $
  \conn -> do
    SQL.execute conn
      "UPDATE reminders \
      \SET notified = 1 \
      \WHERE id = ?"
      [rid]

