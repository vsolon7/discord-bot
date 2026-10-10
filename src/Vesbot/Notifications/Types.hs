module Vesbot.Notifications.Types where

import Vesbot.Parsing (utcToInt, intToUTC)
import Vesbot.Utils (showT)
import Vesbot.Database.Types ()

import Discord.Types

import Data.Maybe (fromMaybe)
import Control.Concurrent.MVar (MVar, takeMVar, putMVar)
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQL


type NotifierControl = MVar ()

sleep :: NotifierControl -> IO ()
sleep = takeMVar

wake :: NotifierControl -> IO ()
wake = \n -> putMVar n ()


data Holiday = Holiday
  { holidayName :: T.Text
  , holidayBlurb :: T.Text
  , holidayDate :: UTCTime
  } deriving Show


data Notification = Notification
  { notificationChannelId :: ChannelId
  , notificationContent :: T.Text
  , notificationReply :: Maybe MessageId
  , notificationCreated :: UTCTime
  , notificationDue :: UTCTime
  } deriving Show


instance SQL.ToRow Notification where
  toRow (Notification cid content replyid created due) =
    [ SQL.SQLText (showT cid)
    , SQL.SQLText content
    , fromMaybe SQL.SQLNull (fmap (SQL.SQLText . showT) replyid)
    , SQL.SQLInteger (utcToInt created)
    , SQL.SQLInteger (utcToInt due)
    ]


instance SQL.FromRow Notification where
  fromRow = Notification
              <$> SQL.field
              <*> SQL.field
              <*> SQL.field
              <*> (intToUTC <$> SQL.field)
              <*> (intToUTC <$> SQL.field)
