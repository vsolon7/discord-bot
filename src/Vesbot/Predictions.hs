{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Predictions where

import Vesbot.Logging as Logging (log, LogType(..))
import Vesbot.Utils (liftIO, showT, ackButtonEditAndRemove, ephemeralResponseBasic, void)
import Vesbot.Parsing (utcToInt)
import Vesbot.Config (Env(..))
import Vesbot.Database (DBConnection)
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
            predictionContent pd <> "*.\n" <>
            "Were they correct?"
      in do 
        Logging.log Info $ "Announcing the due prediction with ID " <> showT pid <> "..."
        let ref = MessageReference (Just mid) (Just cid) Nothing False
            trueButton = Button ("prediction:" <> showT pid <> ":T") False ButtonStyleSuccess (Just "Yes") Nothing
            falseButton = Button ("prediction:" <> showT pid <> ":F") False ButtonStyleDanger (Just "No") Nothing
            actionRow = ActionRowButtons [trueButton, falseButton]
            opts = R.MessageDetailedOpts content False Nothing Nothing [] Nothing (Just ref) (Just [actionRow]) Nothing
        r <- restCall $ R.CreateMessageDetailed cid opts
        case r of
          Left err -> Logging.log Error $ "Failed to send a prediction announcement. Discord returned the error:\n" <> showT err
          _ -> Logging.log Info $ "Successfully announced the prediction to channel " <> showT cid <> "."

  liftIO $ markAsNotified (envDBConnection env) pid


predictionButtonResponse :: DBConnection -> InteractionId -> InteractionToken -> PredictionId -> UserId -> Message -> T.Text -> DiscordHandler ()
predictionButtonResponse dbconn iid itok pid uid msg torf = do
  mpd <- liftIO $ getPredictionById dbconn pid
  case mpd of
    Nothing -> return ()
    Just pd -> do
      if predictionUserId pd /= uid
        then do
          let reply = ephemeralResponseBasic "Only the original predictor can declare the validity of their prediction."
          void . restCall $ R.CreateInteractionResponse iid itok reply
        else do
          let result = if torf == "T" then "correct" else "incorrect"
              reply = "**Marked this prediction as having been " <> result <> ".**"
          ackButtonEditAndRemove iid itok msg reply
          liftIO $ markResult dbconn pid (torf == "T")

