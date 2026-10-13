{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Vesbot.Predictions.Database
  ( initTable
  , savePrediction
  , nextPredictionTime
  , getPredictionById
  , markAsNotified
  ) where

import Vesbot.Predictions.Types
import Vesbot.Database.Types
import Vesbot.Parsing (intToUTC)

import Discord.Internal.Types

import Data.Int (Int64)
import qualified Database.SQLite.Simple as SQL
import qualified Data.Text as T


initTable :: DBConnection -> IO ()
initTable dbconn = withDB dbconn $ \conn ->
  SQL.execute_ conn
    "CREATE TABLE IF NOT EXISTS predictions(\
    \  id             INTEGER PRIMARY KEY,\
    \  guild_id       TEXT NOT NULL,\
    \  user_id        TEXT NOT NULL,\
    \  content        TEXT NOT NULL,\
    \  confidence     REAL,\
    \  created_at     INTEGER NOT NULL,\
    \  due_at         INTEGER NOT NULL,\
    \  notified       INTEGER NOT NULL DEFAULT 0) STRICT"

savePrediction :: DBConnection -> PredictionData -> IO Int64
savePrediction dbconn p = withDB dbconn $
  \conn -> do
    r <- SQL.query conn
      "INSERT INTO predictions \
      \(guild_id, user_id, content, confidence, created_at, due_at) \
      \VALUES (?, ?, ?, ?, ?, ?) \
      \RETURNING id" p
    case r of
      [SQL.Only (Just id)] -> return id
      _ -> error "Somehow failed to get primary key after inserting a prediction into the database."

getPredictionById :: DBConnection -> Int64 -> IO (Maybe PredictionData)
getPredictionById dbconn pid = withDB dbconn $
  \conn -> do
    mr <- SQL.query conn
      "SELECT guild_id, user_id, content, confidence, created_at, due_at \
      \FROM predictions \
      \WHERE id = ?"
      [pid] :: IO ([PredictionData])
    case mr of
      (pd:_) -> return (Just pd)
      _      -> return Nothing

nextPredictionTime :: DBConnection -> IO (Maybe UTCTime)
nextPredictionTime dbconn = withDB dbconn $
  \conn -> do
    nextList <- SQL.query_ conn
                  "SELECT MIN(due_at) \
                  \FROM predictions \
                  \WHERE notified = 0"
    case nextList of
      [SQL.Only (Just t)] -> return $ Just (intToUTC t)
      _                   -> return Nothing

markAsNotified :: DBConnection -> Int64 -> IO ()
markAsNotified dbconn pid = withDB dbconn $
  \conn -> do
    SQL.execute conn
      "UPDATE predictions \
      \SET notified = 1 \
      \WHERE id = ?"
      [pid]
