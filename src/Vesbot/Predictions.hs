{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Predictions where

import Vesbot.Logging as Logging (log, LogType(..))
import Vesbot.Utils (sendMessage, liftIO, showT)
import Vesbot.Parsing (utcToInt)
import Vesbot.Config (Env(..))
import Vesbot.Predictions.Types (PredictionData(..))
import Vesbot.Predictions.Database
import Data.Int (Int64)

import Discord
import Discord.Types
import qualified Discord.Requests as R

import qualified Data.Text as T

type PredictionId = Int64 -- The primary key of the prediction in the database

announceDuePrediction :: Env -> PredictionId -> ChannelId -> MessageId -> DiscordHandler ()
announceDuePrediction env pid cid mid = do
  mpd <- liftIO $ getPredictionById (envDBConnection env) pid
  case mpd of
    Nothing -> Logging.log Error $ "Tried to announce the due prediction with ID" <> showT pid <> ", but \
      \there are no predictions in the database corresponding to that prediction ID!"
    Just pd ->
      let userPing = "<@" <> showT (predictionUserId pd) <> ">"
          predDateUTC = showT . utcToInt $ predictionMadeDate pd
          predDateTimeStampRel = "<t:" <> predDateUTC <> ":R>"
          predDateTimeStampAbs = "<t:" <> predDateUTC <> ":f>"
          claim = case predictionConfidence pd of
                          Nothing -> "claimed"
                          Just c  -> "was " <> showT c <> "%" <> " confident"
          content =
            predDateTimeStampRel <> ", on " <> predDateTimeStampAbs <> ", " <> userPing <> " " <> claim <>
            " that on or before this time today, the following would happen:\n" <> "*" <>
            (predictionContent pd) <> "*.\n" <>
            "Were they correct?"
      in do 
        Logging.log Info $ "Announcing the due prediction with ID " <> showT pid <> "..."
        sendMessage cid (Just mid) content
  liftIO $ markAsNotified (envDBConnection env) pid
