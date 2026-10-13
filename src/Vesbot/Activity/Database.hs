{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Activity.Database
  ( DBConnection
  , withDB
  , initUserActivityTable
  , initEmojiUseTable
  , incrementUserReactionActivity
  , decrementUserReactionActivity
  , incrementUserMessageActivity
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
    \  guild_id         TEXT NOT NULL,\
    \  user_id          TEXT NOT NULL,\
    \  num_messages     INTEGER NOT NULL DEFAULT 0,\
    \  num_reactions    INTEGER NOT NULL DEFAULT 0,\
    \  reactions_score   REAL NOT NULL DEFAULT 0,\
    \  messages_score   REAL NOT NULL DEFAULT 0,\
    \  PRIMARY KEY      (guild_id, user_id)) STRICT"

initEmojiUseTable :: DBConnection -> IO ()
initEmojiUseTable dbconn = withDB dbconn $ \conn -> do
  SQL.execute_ conn
    "CREATE TABLE IF NOT EXIST emoji_use_data(\
    \  guild_id         TEXT NOT NULL,\
    \  emoji_id         TEXT NOT NULL,\
    \  use_time         INTEGER NOT NULL) STRICT"

  SQL.execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_use_time \
    \ON emoji_use_data (use_time);"


clearAllUserActivityData :: DBConnection -> IO ()
clearAllUserActivityData dbconn = withDB dbconn $ \conn ->
  SQL.execute_ conn "DELETE FROM user_activity"


getAllUserActivity :: DBConnection -> IO [UserActivity]
getAllUserActivity dbconn = withDB dbconn $ \conn ->
  SQL.query_ conn "SELECT * FROM user_activity"


incrementUserReactionActivity :: DBConnection -> GuildId -> UserId -> Double -> IO ()
incrementUserReactionActivity dbconn gid uid score = withDB dbconn $
  \conn ->
    SQL.executeNamed conn
      "INSERT INTO user_activity (guild_id, user_id, num_reactions, reactions_score) VALUES (:gid, :uid, 1, :score)\
      \ON CONFLICT(guild_id, user_id)\
      \DO UPDATE SET num_reactions = num_reactions + 1, reactions_score = reactions_score + :score"
      [ ":uid" SQL.:= (showT uid), ":gid" SQL.:= (showT gid), ":score" SQL.:= score ]


decrementUserReactionActivity :: DBConnection -> GuildId -> UserId -> Double -> IO ()
decrementUserReactionActivity dbconn gid uid score = withDB dbconn $
  \conn ->
    SQL.executeNamed conn
      "INSERT INTO user_activity (guild_id, user_id, num_reactions, reactions_score) VALUES (:gid, :uid, 0, 0)\
      \ON CONFLICT(guild_id, user_id)\
      \DO UPDATE SET num_reactions = MAX(num_reactions - 1, 0), reactions_score = MAX(reactions_score - :score, 0)"
      [ ":uid" SQL.:= (showT uid), ":gid" SQL.:= (showT gid), ":score" SQL.:= score ]


incrementUserMessageActivity :: DBConnection -> GuildId -> UserId -> Double -> IO ()
incrementUserMessageActivity dbconn gid uid score = withDB dbconn $
  \conn ->
    SQL.executeNamed conn
      "INSERT INTO user_activity (guild_id, user_id, num_messages, messages_score) VALUES (:gid, :uid, 1, :score)\
      \ON CONFLICT(guild_id, user_id)\
      \DO UPDATE SET num_messages = num_messages + 1, messages_score = messages_score + :score"
      [ ":uid" SQL.:= (showT uid), ":gid" SQL.:= (showT gid), ":score" SQL.:= score ]


updateEmojiUse :: DBConnection -> GuildId -> T.Text -> UTCTime -> IO ()
updateEmojiUse dbconn gid eid t = withDB dbconn $
  \conn -> do
    SQL.execute conn
      "INSERT INTO emoji_use_data (guild_id, emoji_id, use_time) VALUES (?, ?, ?)"
      (showT gid, eid, utcToInt t)


-- getEmojiActivity :: DBConnection -> T.Text -> UTCTime -> IO EmojiActivity
-- getEmojiActivity dbconn eid since = withDB dbconn $
--  \conn -> do
--    rows <- SQL.execute conn
--      "SELECT use_time FROM emoji_use_data \
--      \INDEXED BY idx_use_time \
--      \WHERE use_time >= ?"
--      [utcToInt since]
