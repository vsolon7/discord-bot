{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Vesbot.Predictions.Database where

import Discord.Internal.Types
import Data.Typeable
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Int (Int64)
import Data.Word (Word64)
import Control.Concurrent.MVar
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.Internal as SQL
import qualified Database.SQLite.Simple.Ok as SQL
import qualified Database.SQLite.Simple.FromRow as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Data.Text as T

import Vesbot.Predictions.Types
import Vesbot.Database
import Vesbot.Parsing.Time (formatUTCTime)
import Vesbot.Utils (showT)


instance SQL.ToRow PredictionData where
  toRow (PredictionData pcd pmid) =
    [ SQL.SQLText (predictionContent pcd)
    , fromMaybe SQL.SQLNull (fmap (SQL.SQLInteger . fromInteger) (predictionConfidence pcd))
    , SQL.SQLInteger (fromInteger . formatUTCTime . predictionMadeDate $ pcd)
    , SQL.SQLInteger (fromInteger . formatUTCTime . predictionDueDate $ pcd)
    , SQL.SQLText (showT . predictionUserId $ pcd)
    , SQL.SQLText (showT . predictionGuild $ pcd)
    , SQL.SQLText (showT . predictionChannel $ pcd)
    , SQL.SQLText (showT pmid)
    ]

-- I don't understand this code very well
instance SQL.FromRow PredictionData where
  fromRow = do
    cmdData <- PredictionCommandData
      <$> SQL.field
      <*> SQL.field
      <*> timeField
      <*> timeField
      <*> idField
      <*> idField
      <*> idField
    PredictionData cmdData <$> idField
      where
        timeField :: SQL.RowParser UTCTime
        timeField = posixSecondsToUTCTime . fromIntegral <$> (SQL.field :: SQL.RowParser Int64)
        idField :: Typeable a => SQL.RowParser (DiscordId a)
        idField = SQL.fieldWith $ \f -> do
          t <- SQL.fromField f :: SQL.Ok T.Text
          case readMaybe (T.unpack t) :: Maybe Word64 of
            Just w  -> pure (DiscordId (Snowflake w))
            Nothing -> SQL.returnError SQL.ConversionFailed f "not a valid snowflake"

initPredictionTable :: SQL.Connection -> IO ()
initPredictionTable conn =
  SQL.execute_ conn
    "CREATE TABLE IF NOT EXISTS predictions(\
    \  id             INTEGER PRIMARY KEY,\
    \  content        TEXT NOT NULL,\
    \  confidence     INTEGER,\
    \  created_at     INTEGER NOT NULL,\
    \  due_at         INTEGER NOT NULL,\
    \  user_id        TEXT NOT NULL,\
    \  guild_id       TEXT NOT NULL,\
    \  channel_id     TEXT NOT NULL,\
    \  reply_mess_id  TEXT NOT NULL,\
    \  notified       INTEGER NOT NULL DEFAULT 0) STRICT"


savePrediction :: DbConnection -> MVar () -> PredictionData -> IO ()
savePrediction dbconn wake p = withDb dbconn $
  \conn -> do
    SQL.execute conn 
      "INSERT INTO predictions \
      \(content, confidence, created_at, due_at, user_id, guild_id, channel_id, reply_mess_id) \
      \VALUES (?,?,?,?,?,?,?,?)" p

    -- Wake the prediction notifier so that it can recompute when the next prediction is due.
    void (tryPutMVar wake ())


nextPredictionTime :: DbConnection -> IO (Maybe UTCTime)
nextPredictionTime dbconn = withDb dbconn $
  \conn -> do
    nextList <- SQL.query_ conn
                  "SELECT MIN(due_at) \
                  \FROM predictions \
                  \WHERE notified = 0" :: IO [SQL.Only (Maybe Integer)]
    case nextList of
      [SQL.Only (Just t)] -> return $ Just (posixSecondsToUTCTime . fromInteger $ t)
      _                   -> return Nothing


duePredictions :: DbConnection -> IO [PredictionData]
duePredictions dbconn = withDb dbconn $
  \conn -> do
    nowPOSIX <- fmap formatUTCTime getCurrentTime :: IO Integer
    duePreds <-
      SQL.query conn
        "SELECT content, confidence, created_at, due_at, user_id, guild_id, channel_id, reply_mess_id \
        \FROM predictions \
        \WHERE notified = 0 AND due_at <= ?"
        (SQL.Only nowPOSIX) :: IO [PredictionData]

    SQL.execute conn
      "UPDATE predictions SET notified = 1 \
      \WHERE notified = 0 AND due_at <= ?"
      (SQL.Only nowPOSIX)

    return duePreds
