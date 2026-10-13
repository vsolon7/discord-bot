{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Vesbot.Predictions.Types where

import Vesbot.Utils (showT)
import Vesbot.Parsing (utcToInt, intToUTC)
import Vesbot.Database.Types()

import Discord.Types

import Data.Time()
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQL
import Data.Maybe (fromMaybe)

-- | Record to hold all of the information associated to the prediction slash command when the user
-- sends it to the bot
data PredictionData = PredictionData
  { predictionGuildId :: GuildId
  , predictionUserId :: UserId
  , predictionContent :: T.Text
  , predictionConfidence :: Maybe Double
  , predictionMadeDate :: UTCTime
  , predictionDueDate :: UTCTime
  } deriving Show


instance SQL.ToRow PredictionData where
  toRow pd  =
    [ SQL.SQLText (showT . predictionGuildId $ pd)
    , SQL.SQLText (showT . predictionUserId $ pd)
    , SQL.SQLText (predictionContent pd)
    , fromMaybe SQL.SQLNull (fmap SQL.SQLFloat (predictionConfidence pd))
    , SQL.SQLInteger (utcToInt . predictionMadeDate $ pd)
    , SQL.SQLInteger (utcToInt . predictionDueDate $ pd)
    ]

-- see Vesbot.Database.Types for the FromField DiscordId instance
instance SQL.FromRow PredictionData where
  fromRow =
    PredictionData
      <$> SQL.field
      <*> SQL.field
      <*> SQL.field
      <*> SQL.field
      <*> (intToUTC <$> SQL.field)
      <*> (intToUTC <$> SQL.field)
