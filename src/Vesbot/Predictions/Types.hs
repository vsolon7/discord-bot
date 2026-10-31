module Vesbot.Predictions.Types where

import Vesbot.Utils (showT)
import Vesbot.Parsing (utcToInt, intToUTC)
import Vesbot.Database.Types()

import Discord.Types

import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQL
import Data.Int(Int64)

type PredictionId = Int64 -- The primary key of the prediction in the database

-- | Record to hold all of the information associated to the prediction slash command when the user
-- sends it to the bot
data PredictionData = PredictionData
  { predictionUserId :: UserId
  , predictionContent :: T.Text
  , predictionConfidence :: Maybe Double
  , predictionMadeDate :: UTCTime
  , predictionDueDate :: UTCTime
  } deriving Show


instance SQL.ToRow PredictionData where
  toRow (PredictionData uid content confidence made due)  =
    [ SQL.SQLText (showT uid)
    , SQL.SQLText content
    , maybe SQL.SQLNull SQL.SQLFloat confidence
    , SQL.SQLInteger (utcToInt made)
    , SQL.SQLInteger (utcToInt due)
    ]

-- see Vesbot.Database.Types for the FromField DiscordId instance
instance SQL.FromRow PredictionData where
  fromRow =
    PredictionData
      <$> SQL.field
      <*> SQL.field
      <*> SQL.field
      <*> (intToUTC <$> SQL.field)
      <*> (intToUTC <$> SQL.field)
