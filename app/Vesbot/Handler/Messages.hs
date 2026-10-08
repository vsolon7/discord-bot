{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Handler.Messages where

import Discord.Types
import qualified Database.SQLite.Simple as SQL

import Vesbot.Database
import Vesbot.Utils (showT)
import Vesbot.Parsing.Time (formatUTCTime)


updateMessageActivity :: DbConnection -> GuildId -> UserId -> UTCTime -> IO ()
updateMessageActivity dbconn gid uid now = withDb dbconn $
  \conn ->
    SQL.executeNamed conn
      "INSERT INTO activity (guild_id, user_id, last_message_at, num_messages) VALUES (:gid, :uid, :now, 1)\
      \ON CONFLICT(guild_id, user_id)\
      \DO UPDATE SET last_message_at = :now, num_messages = num_messages + 1"
      [ ":now" SQL.:= (formatUTCTime now), ":uid" SQL.:= (showT uid), ":gid" SQL.:= (showT gid) ]
