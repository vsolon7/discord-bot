{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Predictions where

import Vesbot.Logging as Logging (log, LogType(..))
import Vesbot.Utils (liftIO, showT, ackButtonEditAndRemove, ephemeralResponseBasic, void)
import Vesbot.Parsing (utcToInt)
import Vesbot.Config (Env(..))
import Vesbot.Database (DBConnection)
import Vesbot.Predictions.Types (PredictionId, PredictionData(..))
import Vesbot.Predictions.Database

import Discord
import Discord.Types
import qualified Discord.Requests as R

import qualified Data.Text as T

announceDuePrediction :: Env -> PredictionId -> ChannelId -> MessageId -> DiscordHandler ()
announceDuePrediction env pid cid mid = do
  Logging.log Info $ T.intercalate "\n"
    [ "Attempting to announce a due prediction with the following data:"
    , logInfo
    ]
  mpd <- liftIO $ getPredictionById (envDBConnection env) pid
  case mpd of
    Nothing ->
      Logging.log Error $ "Could not find the prediction ID in the database."
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
        let ref = MessageReference (Just mid) (Just cid) Nothing False
            trueButton = Button ("prediction:" <> showT pid <> ":T") False ButtonStyleSuccess (Just "Yes") Nothing
            falseButton = Button ("prediction:" <> showT pid <> ":F") False ButtonStyleDanger (Just "No") Nothing
            actionRow = ActionRowButtons [trueButton, falseButton]
            opts = R.MessageDetailedOpts content False Nothing Nothing [] Nothing (Just ref) (Just [actionRow]) Nothing
        r <- restCall $ R.CreateMessageDetailed cid opts
        case r of
          Left err ->
            Logging.log Error $
              "The REST call to send the message failed. Discord returned the error:\n" <> showT err
          _ -> do
            liftIO $ markAsNotified (envDBConnection env) pid
            Logging.log Info $
              "Successfully announced the due prediction."
  where
    logInfo = T.intercalate "\n"
      ["PREDICTION ID: " <> showT pid
      ,"CHANNEL ID: " <> showT cid
      ,"ORIGINAL MESSAGE ID: " <> showT mid
      ]


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

