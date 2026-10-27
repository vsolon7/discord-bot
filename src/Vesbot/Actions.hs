{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Vesbot.Actions where

import Discord
import Discord.Types

import Data.Aeson ()
import GHC.Generics (Generic)
import Data.Int (Int64)

import qualified Data.Text as T


data Action
  = SendAnnouncement T.Text
  | SendMessage ChannelId (Maybe MessageId) T.Text
  | CurrencyDrop ChannelId Double T.Text
  | AnnounceDuePrediction Int64 ChannelId MessageId
  | AnnounceDueWager Int64 ChannelId MessageId
  | SendReminder Int64 ChannelId MessageId
  | RemoveButtons MessageId
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
