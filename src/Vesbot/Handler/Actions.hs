{-# LANGUAGE LambdaCase #-}
module Vesbot.Handler.Actions
  ( actionHandler
  ) where

import Vesbot.Utils (announce, sendMessage)
import Vesbot.Config (Env(..), Config(..))
import Vesbot.Actions
import Vesbot.Predictions (announceDuePrediction)
import Vesbot.Reminders (sendReminder)

import Discord

actionHandler :: Env -> Config -> Action -> DiscordHandler ()
actionHandler env cfg =
  \case
    SendAnnouncement content -> announce (cfgAnnouncementCID cfg) content
    SendMessage cid replyTo content -> sendMessage cid replyTo content
    AnnounceDuePrediction pid cid replyTo -> announceDuePrediction env pid cid replyTo
    SendReminder rid cid replyTo -> sendReminder env rid cid replyTo
  --  CurrencyDrop cid amt text -> return ()
  --  AnnounceDueWager id cid replyTo -> return ()
  --  RemoveButtons mid -> return ()
    _ -> return ()
