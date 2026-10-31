{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Reminders where

import Vesbot.Logging as Logging (log, LogType(..))
import Vesbot.Utils (liftIO, showT)
import Vesbot.Parsing (utcToInt)
import Vesbot.Config (Env(..))
import Vesbot.Reminders.Types (ReminderId, ReminderData(..))
import Vesbot.Reminders.Database

import Discord
import Discord.Types
import qualified Discord.Requests as R

import qualified Data.Text as T

sendReminder :: Env -> ReminderId -> ChannelId -> MessageId -> DiscordHandler ()
sendReminder env rid cid mid = do
  Logging.log Info $ T.intercalate "\n"
    [ "Attempting to send a reminder with the following data:"
    , logInfo
    ]
  mrd <- liftIO $ getReminderById (envDBConnection env) rid
  case mrd of
    Nothing ->
      Logging.log Error $ "Could not find the reminder ID in the database."
    Just rd ->
      let ping = "<@" <> showT (reminderUserId rd) <> ">"
          predDateUTC = showT . utcToInt $ reminderMadeDate rd
          predDateTimeStampAbs = "<t:" <> predDateUTC <> ":f>"
          content =
            ping <> "\nOn" <> predDateTimeStampAbs <> ", " <>
            "you asked for me to send you the following reminder:\n" <> "*" <>
            reminderContent rd <> "*."
      in do 
        let ref = MessageReference (Just mid) (Just cid) Nothing False
            opts = R.MessageDetailedOpts content False Nothing Nothing [] Nothing (Just ref) Nothing Nothing
        r <- restCall $ R.CreateMessageDetailed cid opts
        case r of
          Left err ->
            Logging.log Error $
              "The REST call to send the message failed. Discord returned the error:\n" <> showT err 
          _ -> do
            liftIO $ markAsNotified (envDBConnection env) rid
            Logging.log Info $
              "Successfully sent the reminder."
  where
    logInfo = T.intercalate "\n"
      ["REMINDER ID: " <> showT rid
      ,"CHANNEL ID: " <> showT cid
      ,"MESSAGE ID: " <> showT mid
      ]
