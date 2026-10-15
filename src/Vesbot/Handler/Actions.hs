{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Handler.Actions
  ( actionHandler
  ) where

import Vesbot.Utils (void, showT, announce, sendMessage)
import Vesbot.Logging as Logging (log, LogType(..))
import Vesbot.Config (Env(..), Config(..))
import Vesbot.Actions
import Vesbot.Database.Types (DBConnection)
import Vesbot.Predictions (announceDuePrediction)

import Discord
import Discord.Types
import qualified Discord.Requests as R

import qualified Data.Text as T

actionHandler :: Env -> Config -> Action -> DiscordHandler ()
actionHandler env cfg = \a -> case a of
  SendAnnouncement content -> announce (cfgAnnouncementCID cfg) content
  SendMessage cid replyTo content -> sendMessage cid replyTo content
  CurrencyDrop cid amt text -> return ()
  AnnounceDuePrediction pid cid replyTo -> announceDuePrediction env pid cid replyTo
  AnnounceDueWager id cid replyTo -> return ()
  SendReminder id cid replyTo -> return ()
  RemoveButtons mid -> return ()
  _ -> return ()
