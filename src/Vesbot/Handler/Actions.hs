{-# LANGUAGE LambdaCase #-}
module Vesbot.Handler.Actions
  ( actionHandler
  ) where

import Vesbot.Utils (announce, sendMessage)
import Vesbot.Config (Env(..), Config(..))
import Vesbot.Actions
-- import Vesbot.Predictions (announceDuePrediction)

import Discord

actionHandler :: Env -> Config -> Action -> DiscordHandler ()
actionHandler env cfg =
  \case
    SendAnnouncement content -> announce (cfgAnnouncementCID cfg) content
    SendMessage cid replyTo content -> sendMessage cid replyTo content
  --  CurrencyDrop cid amt text -> return ()
  --  AnnounceDuePrediction pid cid replyTo -> announceDuePrediction env pid cid replyTo
  --  AnnounceDueWager id cid replyTo -> return ()
  --  SendReminder id cid replyTo -> return ()
  --  RemoveButtons mid -> return ()
    _ -> return ()
