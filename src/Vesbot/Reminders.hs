{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Reminders where

import Vesbot.Logging as Logging (log, LogType(..))
import Vesbot.Utils (liftIO, showT)
import Vesbot.Parsing (utcToInt)
import Vesbot.Config (Env(..))
import Vesbot.Reminders.Types (ReminderData(..))
import Vesbot.Reminders.Database
import Data.Int (Int64)

import Discord
import Discord.Types
import qualified Discord.Requests as R

type ReminderId = Int64 -- The primary key of the reminder in the database

sendReminder :: Env -> ReminderId -> ChannelId -> MessageId -> DiscordHandler ()
sendReminder env rid cid mid = do
  mrd <- liftIO $ getReminderById (envDBConnection env) rid
  case mrd of
    Nothing -> Logging.log Error $ "Tried to announce the due reminder with ID" <> showT rid <> ", but \
      \there are no reminders in the database corresponding to that reminder ID!"
    Just rd ->
      let ping = "<@" <> showT (reminderUserId rd) <> ">"
          predDateUTC = showT . utcToInt $ reminderMadeDate rd
          predDateTimeStampRel = "<t:" <> predDateUTC <> ":R>"
          predDateTimeStampAbs = "<t:" <> predDateUTC <> ":f>"
          content =
            ping <> "\n" <> predDateTimeStampRel <> ", on " <> predDateTimeStampAbs <> ", " <>
            "you asked for me to send you the following reminder:\n" <> "*" <>
            reminderContent rd <> "*."
      in do 
        Logging.log Info $ "Announcing the due reminder with ID " <> showT rid <> "..."
        let ref = MessageReference (Just mid) (Just cid) Nothing False
            opts = R.MessageDetailedOpts content False Nothing Nothing [] Nothing (Just ref) Nothing Nothing
        r <- restCall $ R.CreateMessageDetailed cid opts
        case r of
          Left err -> Logging.log Error $ "Failed to send a reminder reply. Discord returned the error:\n" <> showT err
          _ -> Logging.log Info $ "Successfully sent a reminder to channel " <> showT cid <> "."

  liftIO $ markAsNotified (envDBConnection env) rid

