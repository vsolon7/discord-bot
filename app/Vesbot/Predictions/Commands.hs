{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Predictions.Commands where

import Discord
import Discord.Types
import Discord.Internal.Types.Interactions
import Discord.Internal.Types.ApplicationCommands
import qualified Discord.Requests as R
import Data.Time
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)
import qualified Data.Text as T

import Vesbot.Utils
import Vesbot.Parsing.Time
import Vesbot.Predictions.Types
import Vesbot.Predictions.Database
import Vesbot.SlashCommands.Types


parsePredictionCommand :: UTCTime -> Interaction -> Either T.Text PredictionCommandData
parsePredictionCommand currTime (cmd@InteractionApplicationCommand { applicationCommandData = input@ApplicationCommandDataChatInput {} }) =
  do
    dataValues <-
      maybeToEither
        "Could not access the prediction command's option fields."
        (optionsData input)
    let
      confidence = -- :: Maybe Integer, since Nothing is OK; confidence level isn't a required input
        extractIntegerOption
          "confidence"
          dataValues
    predText <-
      maybeToEither
        "Could not access prediction field."
        (extractStringOption "prediction" dataValues)
    dueDateText <-
      maybeToEither
        "Could not access date field."
        (extractStringOption "date" dataValues)
    gid <-
      maybeToEither
        "Error accessing Guild ID. Note that this command can only be used in a server."
        (interactionGuildId cmd)
    cid <-
      maybeToEither
        "Error accessing Channel ID."
        (interactionChannelId cmd)
    uid <-
      maybeToEither
        "Could not get user data. Note that this command can only be used in a server."
        (getUserData . interactionUser $ cmd)
    dueDateUTC <-
      maybeToEither
        "Error parsing prediction date. Here are the formatting options.\n\
        \**Relative time:** \"in #w\" or \"in #d\" or \"in #h\" or \"in #m\", where # is a positive integer.\
        \ \"w\" stands for weeks, \"d\" stands for days, \"h\" stands for hours, and \"m\" stands for minutes.\n\
        \**Absolute time:** \"on month-day-year HH:MM <timezone>\", where the timezone is optional.\
        \If no timezone is entered, the bot will default to UTC. Timezone examples are CST, CDT, EST, etc."
        (parseDateInput currTime . T.unpack $ dueDateText)
    return $
      PredictionCommandData
        { predictionContent = predText
        , predictionConfidence = confidence
        , predictionMadeDate = currTime
        , predictionDueDate = dueDateUTC
        , predictionUserId = uid
        , predictionGuild = gid
        , predictionChannel = cid
        }

parsePredictionCommand _ _ = Left "Tried to parse an interaction that is not a chat input slash command when parsing the prediction command."


-- | Generate the bot reply. parsePredictionCommand returns either an error message or a record of the
-- relevant prediction command data. We must either respond with an (ephemeral) error, or else form the bot reply
-- in the case that the user actually predicted a future event.
createInitialPredictionResponse :: UTCTime -> Either T.Text PredictionCommandData -> InteractionResponse
createInitialPredictionResponse currTime pcd =
  case pcd of
    Right p ->
      if currTime < predictionDueDate p then
        let
          userPing = "<@" <> showT (predictionUserId p) <> ">"
          dueDateUTC = showT . formatUTCTime $ predictionDueDate p
          dueDateTimeStampRel = "<t:" <> dueDateUTC <> ":R>"
          dueDateTimeStampAbs = "<t:" <> dueDateUTC <> ":f>"
          responseStart = case predictionConfidence p of
            Nothing -> "They claim"
            Just c  -> "They are " <> showT c <> "%" <> " confident"
          response =
            userPing <> " has made a prediction!\n" <>
            responseStart <> " that on or before " <> dueDateTimeStampAbs <> ", which is " <> dueDateTimeStampRel <> 
            ", the following will occur:\n" <>
            predictionContent p <> "."
        in
          interactionResponseBasic response
      else
        ephemeralResponseBasic "You can only predict events in the future!"
    Left err -> ephemeralResponseBasic err


-- | This giant function just creates the response message that the bot will reply with.
createPredictionAnnouncement :: PredictionData -> R.ChannelRequest Message
createPredictionAnnouncement (PredictionData p mid) =
  let
    userPing = "<@" <> showT (predictionUserId p) <> ">"
    predDateUTC = showT . formatUTCTime $ predictionMadeDate p
    predDateTimeStampRel = "<t:" <> predDateUTC <> ":R>"
    predDateTimeStampAbs = "<t:" <> predDateUTC <> ":f>"
    claim = case predictionConfidence p of
                      Nothing -> "claimed"
                      Just c  -> "was " <> showT c <> "%" <> " confident"
    content =
      predDateTimeStampRel <> ", on " <> predDateTimeStampAbs <> ", " <> userPing <> " " <> claim <>
      " that on or before this time today, the following would happen:\n" <> "*" <>
      (predictionContent p) <> "*.\n" <>
      "Were they correct?"
    mref =
      Just $
        MessageReference
          (Just mid)
          (Just (predictionChannel p))
          (Just (predictionGuild p))
          True
    in
      R.CreateMessageDetailed
        (predictionChannel p)
        (R.MessageDetailedOpts
          content
          False
          Nothing
          Nothing
          Nothing
          mref
          Nothing
          Nothing)


makePrediction :: SlashCommand
makePrediction = SlashCommand
  { commandName = "predict"
  , commandRegistration = Just reg
  , commandHandler = \dbconn wake intr _ -> do
      currTime <- liftIO getCurrentTime
      let iTok = interactionToken intr -- save the interaction token to get the message ID of the reply later
      let pcd = parsePredictionCommand currTime intr :: Either T.Text PredictionCommandData
      let botReply = createInitialPredictionResponse currTime pcd

      -- Respond with the error or the correct reply
      void . restCall $
        R.CreateInteractionResponse
          (interactionId intr)
          (interactionToken intr)
          botReply

      -- | If the predictionCommandData was parsed correctly, we want to save the prediction to the database
      -- We can't do this before because we also want to save the ID of the message the bot responds to the
      -- slash command with, and this message is sent **after** the restCall immediately above.
      case pcd of
        Right p | currTime < predictionDueDate p -> do
          maybeResult <- restCall $
            R.GetOriginalInteractionResponse
              (interactionApplicationId intr)
              iTok
          case maybeResult of
            Right msg -> do
              void . liftIO . forkIO $ do -- forkIO is probably not necessary.
                let predictionData = PredictionData p (messageId msg)
                savePrediction dbconn wake predictionData
            Left err -> echo $
              "Error acquiring the bot's prediction reply message.\
              \Discord sent the following error message:\n" <> showT err
        _ -> return ()
  }
    where
      reg = -- Command registration
        CreateApplicationCommandChatInput
          "predict"
          Nothing
          "Predict the future!"
          Nothing (
            Just (
              OptionsValues
                [
                  OptionValueString
                    "prediction"
                    Nothing
                    "What you are predicting will happen"
                    Nothing
                    True
                    (Left False)
                    (Just 1)
                    Nothing
                , OptionValueString
                    "date"
                    Nothing
                    "\"in #[w|d|h|m]\" or \"on MM-DD-YYYY HH:MM <timezone>\""
                    Nothing
                    True
                    (Left False)
                    (Just 1)
                    Nothing
                , OptionValueInteger
                    "confidence"
                    Nothing
                    "(Optional) Prediction confidence"
                    Nothing
                    False
                    (Left False)
                    (Just 1)
                    (Just 100)
                ]
              )
            )
          Nothing
          (Just False)
