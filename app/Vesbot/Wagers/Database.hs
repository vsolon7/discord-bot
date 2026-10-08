{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Wagers.Database where

import Data.Maybe (fromMaybe)
import qualified Database.SQLite.Simple as SQL

import Vesbot.Utils (showT)
import Vesbot.Parsing.Time (formatUTCTime)
import Vesbot.Database()
import Vesbot.Wagers.Types


initWagerTable :: SQL.Connection -> IO ()
initWagerTable conn =
  SQL.execute_ conn
    "CREATE TABLE IF NOT EXISTS wagers(\
    \  id             INTEGER PRIMARY KEY,\
    \  content        TEXT NOT NULL,\
    \  bet_amount     REAL NOT NULL,\
    \  wager_odds     REAL,\
    \  due_at         INTEGER NOT NULL,\
    \  offering_user  TEXT NOT NULL,\
    \  guild_id       TEXT NOT NULL,\
    \  channel_id     TEXT NOT NULL,\
    \  accepting_user TEXT NOT NULL,\
    \  accepted_at    INTEGER NOT NULL,\
    \  reply_mess_id  TEXT NOT NULL,\
    \  notified       INTEGER NOT NULL DEFAULT 0) STRICT"
