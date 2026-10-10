module Vesbot.Predictions.Types where

import Discord.Types
import qualified Data.Text as T
import Data.Time (UTCTime)

data Prediction = Prediction
  { predUser :: UserId
  , predChannelId :: ChannelId
  , predContent :: T.Text
  , predConfidence :: Maybe Double
  , predMadeDate :: UTCTime
  , predDueDate :: UTCTime
  , predMessageId :: MessageId
  } deriving Show

