{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Vesbot.Predictions.Types where

import Discord.Types
import Data.Time()
import qualified Data.Text as T


-- | Record to hold all of the information associated to the prediction slash command when the user
-- sends it to the bot
data PredictionCommandData = PredictionCommandData
  { predictionContent :: T.Text
  , predictionConfidence :: Maybe Integer
  , predictionMadeDate :: UTCTime
  , predictionDueDate :: UTCTime
  , predictionUserId :: UserId
  , predictionGuild :: GuildId
  , predictionChannel :: ChannelId
  } deriving Show

-- | This record adjoins the message ID of the bot's reply to the prediction slash command in the channel.
data PredictionData = PredictionData
  { predictionCommandData :: PredictionCommandData
  , predictionReplyMessageId :: MessageId
  } deriving Show
