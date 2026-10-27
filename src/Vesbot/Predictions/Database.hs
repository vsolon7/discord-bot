{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Predictions.Database
  ( initTable
  , savePrediction
  , nextPredictionTime
  , getPredictionById
  , markAsNotified
  , markResult
  ) where

import Vesbot.Predictions.Types
import Vesbot.Database.Types
import Vesbot.Parsing (intToUTC)

import Discord.Internal.Types

import Data.Int (Int64)
import qualified Database.SQLite.Simple as SQL


initTable :: DBConnection -> IO ()
initTable dbconn = withDB dbconn $ \conn ->
  SQL.execute_ conn
    "CREATE TABLE IF NOT EXISTS predictions(\
    \  id             INTEGER PRIMARY KEY,\
    \  user_id        TEXT NOT NULL,\
    \  content        TEXT NOT NULL,\
    \  confidence     REAL,\
    \  created_at     INTEGER NOT NULL,\
    \  due_at         INTEGER NOT NULL,\
    \  notified       INTEGER NOT NULL DEFAULT 0,\
    \  came_true      INTEGER) STRICT"

savePrediction :: DBConnection -> PredictionData -> IO Int64
savePrediction dbconn p = withDB dbconn $
  \conn -> do
    r <- SQL.query conn
      "INSERT INTO predictions \
      \(user_id, content, confidence, created_at, due_at) \
      \VALUES (?, ?, ?, ?, ?) \
      \RETURNING id" p
    case r of
      [SQL.Only (Just pid)] -> return pid
      _ -> error "Somehow failed to get primary key after inserting a prediction into the database."

getPredictionById :: DBConnection -> Int64 -> IO (Maybe PredictionData)
getPredictionById dbconn pid = withDB dbconn $
  \conn -> do
    mr <- SQL.query conn
      "SELECT user_id, content, confidence, created_at, due_at \
      \FROM predictions \
      \WHERE id = ?"
      [pid] :: IO [PredictionData]
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

markResult :: DBConnection -> Int64 -> Bool -> IO ()
markResult dbconn pid res = withDB dbconn $
  \conn -> do
    let intres = if res then 1 else 0 :: Int64
    SQL.execute conn
      "UPDATE predictions \
      \SET came_true = ? \
      \WHERE id = ?"
      (intres, pid)
