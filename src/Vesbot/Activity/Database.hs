{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Activity.Database
  ( DBConnection
  , withDB
  , initUserActivityTable
  , initEmojiUseTable
  , modifyReactionActivity
  , incrementMessageActivity
  ) where

import Vesbot.Utils (showT)
import Vesbot.Parsing (utcToInt)
import Vesbot.Database.Types (DBConnection, withDB)
import Vesbot.Activity.Types (UserActivity)

import Discord.Types

import qualified Database.SQLite.Simple as SQL
import qualified Data.Text as T


initUserActivityTable :: DBConnection -> IO ()
initUserActivityTable dbconn = withDB dbconn $ \conn ->
  SQL.execute_ conn
    "CREATE TABLE IF NOT EXISTS user_activity(\
    \  user_id          TEXT NOT NULL PRIMARY KEY,\
    \  num_messages     INTEGER NOT NULL DEFAULT 0,\
    \  num_reactions    INTEGER NOT NULL DEFAULT 0,\
    \  num_reactees     INTEGER NOT NULL DEFAULT 0) WITHOUT ROWID, STRICT"

initEmojiUseTable :: DBConnection -> IO ()
initEmojiUseTable dbconn = withDB dbconn $ \conn -> do
  SQL.execute_ conn
    "CREATE TABLE IF NOT EXIST emoji_use_data(\
    \  emoji_id         TEXT NOT NULL,\
    \  use_time         INTEGER NOT NULL) WITHOUT ROWID STRICT"

  SQL.execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_use_time \
    \ON emoji_use_data (use_time);"


clearAllUserActivityData :: DBConnection -> IO ()
clearAllUserActivityData dbconn = withDB dbconn $ \conn ->
  SQL.execute_ conn "DELETE FROM user_activity"


getAllUserActivity :: DBConnection -> IO [UserActivity]
getAllUserActivity dbconn = withDB dbconn $ \conn ->
  SQL.query_ conn "SELECT * FROM user_activity"


modifyReactionActivity :: DBConnection -> UserId -> UserId -> IO ()
modifyReactionActivity dbconn reacter reactee = withDB dbconn $
  \conn ->
    SQL.withTransaction conn $ do
      SQL.executeNamed conn
        "INSERT INTO user_activity (user_id, num_reactions) VALUES (:reactee, 1)\
        \ON CONFLICT(user_id)\
        \DO UPDATE SET num_reactions = num_reactions + 1"
        [ ":reactee" SQL.:= showT reactee ]
      SQL.executeNamed conn
        "INSERT INTO user_activity (user_id, num_reactees) VALUES (:reacter, 1)\
        \ON CONFLICT(user_id)\
        \DO UPDATE SET num_reactions = num_reactions + 1;"
        [ ":reacter" SQL.:= (showT reacter) ]


incrementMessageActivity :: DBConnection -> UserId -> IO ()
incrementMessageActivity dbconn uid = withDB dbconn $
  \conn ->
    SQL.executeNamed conn
      "INSERT INTO user_activity (user_id, num_messages) VALUES (:uid, 1)\
      \ON CONFLICT(user_id)\
      \DO UPDATE SET num_messages = num_messages + 1"
      [ ":uid" SQL.:= (showT uid) ]


addEmojiUse :: DBConnection -> T.Text -> UTCTime -> IO ()
addEmojiUse dbconn eid t = withDB dbconn $
  \conn -> do
    SQL.execute conn
      "INSERT INTO emoji_use_data (emoji_id, use_time) VALUES (?, ?)"
      (eid, utcToInt t)


-- getEmojiActivity :: DBConnection -> T.Text -> UTCTime -> IO EmojiActivity
-- getEmojiActivity dbconn eid since = withDB dbconn $
--  \conn -> do
--    rows <- SQL.execute conn
--      "SELECT use_time FROM emoji_use_data \
--      \INDEXED BY idx_use_time \
--      \WHERE use_time >= ?"
--      [utcToInt since]
