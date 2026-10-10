{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
module Vesbot.Notifications.Types
  ( NotificationsEnv(..)
  , NotificationDataInternal(..)
  , Waker(..)
  , nudge
  , await
  ) where

import Vesbot.Parsing (utcToInt, intToUTC)
import Vesbot.Utils (showT, void)
import Vesbot.Database.Types (DBConnection)

import Discord.Types

import Data.Maybe (fromMaybe)
import Control.Concurrent.MVar (MVar, takeMVar, tryPutMVar)
import Data.Time
import Data.Time.Calendar
import Data.Time.Calendar.Quarter
import Data.Time.Calendar.OrdinalDate
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQL


-- This type is used to alert the notification system to check for new notifications
newtype Waker = Waker (MVar ())

await :: Waker -> IO ()
await (Waker n) = takeMVar n

nudge :: Waker -> IO ()
nudge (Waker n) = void . tryPutMVar n $ ()


data NotificationsEnv = NotificationsEnv
  { notificationDBConn :: DBConnection 
  , notificationWaker :: Waker
  }

data Single
data Recurring

data NotificationData = NotificationData
  { notifUId :: Maybe T.Text
  , notifChannelId :: ChannelId
  , notifContent :: T.Text
  , notifReply :: Maybe MessageId
  } deriving Show

data Notification a where
  CreateSingleNotification :: NotificationData -> UTCTime -> Notification Single
  CreatePeriodicNotification :: NotificationData -> NominalDiffTime -> Notification Recurring
  CreateYearlyNotification :: NotificationData -> (DayOfYear, TimeOfDay) -> Notification Recurring
  CreateQuarterlyNotification :: NotificationData -> (DayOfQuarter, TimeOfDay) -> Notification Recurring
  CreateMonthlyNotification :: NotificationData -> (DayOfMonth, TimeOfDay) -> Notification Recurring
  CreateWeeklyNotification :: NotificationData -> (DayOfWeek, TimeOfDay) -> Notification Recurring
  CreateDailyNotification :: NotificationData -> TimeOfDay -> Notification Recurring
  CreateRelativeMonthNotication :: NotificationData -> (MonthOfYear, DayOfWeek, Int) -> Notification Recurring

data NotificationDataInternal = NotificationDataInternal
  { internNotifUId :: Maybe T.Text
  , internNotifChannelId :: ChannelId
  , internNotifContent :: T.Text
  , internNotifReply :: Maybe MessageId
  , internNotifDue :: UTCTime
  , internNotifType :: T.Text
  } deriving Show

instance SQL.ToRow NotificationDataInternal where
  toRow (NotificationDataInternal uid cid content replyid due ntype) =
    [ fromMaybe SQL.SQLNull (fmap SQL.SQLText uid)
    , SQL.SQLText (showT cid)
    , SQL.SQLText content
    , fromMaybe SQL.SQLNull (fmap (SQL.SQLText . showT) replyid)
    , SQL.SQLInteger (utcToInt due)
    , SQL.SQLText ntype
    ]

instance SQL.FromRow NotificationDataInternal where
  fromRow = NotificationDataInternal
              <$> SQL.field
              <*> SQL.field
              <*> SQL.field
              <*> SQL.field
              <*> (intToUTC <$> SQL.field)
              <*> SQL.field
