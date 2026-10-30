module Vesbot.Reminders.Types where

import Vesbot.Utils (showT)
import Vesbot.Parsing (utcToInt, intToUTC)
import Vesbot.Database.Types()

import Discord.Types

import Data.Time()
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQL

-- | Record to hold all of the information associated to the prediction slash command when the user
-- sends it to the bot
data ReminderData = ReminderData
  { reminderUserId :: UserId
  , reminderContent :: T.Text
  , reminderMadeDate :: UTCTime
  , reminderDueDate :: UTCTime
  } deriving Show


instance SQL.ToRow ReminderData where
  toRow (ReminderData uid content made due)  =
    [ SQL.SQLText (showT uid)
    , SQL.SQLText content
    , SQL.SQLInteger (utcToInt made)
    , SQL.SQLInteger (utcToInt due)
    ]

-- see Vesbot.Database.Types for the FromField DiscordId instance
instance SQL.FromRow ReminderData where
  fromRow =
    ReminderData
      <$> SQL.field
      <*> SQL.field
      <*> (intToUTC <$> SQL.field)
      <*> (intToUTC <$> SQL.field)
