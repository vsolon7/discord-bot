{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module SlashCommands where

import Discord
import Discord.Interactions
import Discord.Internal.Types.Channel (messageId)
import UnliftIO (liftIO)
import Data.Text (Text)
import Control.Monad (void)
import Control.Concurrent (forkIO, MVar)
import qualified Discord.Requests as R
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQL
import Data.Time (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Utils
import ArgumentTimer (setSavedTime, getTimeDiff, formatDiffTime)
import Predictions
import Wagers

-- TODO: Create different slash command types? Not all slash commands need access to the database or
-- the MVar used to wake the prediction notifier
data SlashCommand = SlashCommand
  { commandName :: Text
  , commandRegistration :: Maybe CreateApplicationCommand
  , commandHandler :: DbConnection
                   -> MVar ()
                   -> Interaction
                   -> Maybe OptionsData
                   -> DiscordHandler ()
  }

-- | Constructor for a basic slash command with no options.
-- it just replies with some text that is the result of running IO actions.
basicSlashCommand :: Text    -> -- Slash Command Name
                     Text    -> -- Registration Description
                     IO Text -> -- Text diplayed in the interaction response, possibly obtained with IO
                     SlashCommand
basicSlashCommand name regDesc statefulText
  = SlashCommand
    { commandName = name
    , commandRegistration = createChatInput name regDesc
    , commandHandler = \_ _ intr _ -> do
        iomessage <- liftIO statefulText
        void . restCall $
          R.CreateInteractionResponse
            (interactionId intr)
            (interactionToken intr)
            (interactionResponseBasic iomessage)
    }

-- List of slash commands to register
mySlashCommands :: [SlashCommand]
mySlashCommands = [ping, getCurrTime, resetArgCounter, viewArgCounter, printGiantGlorp, makePrediction, makeWager]


ping :: SlashCommand
ping =
  basicSlashCommand
    "ping"
    "Responds 'pong'"
    (return "pong!")


getCurrTime :: SlashCommand
getCurrTime =
  basicSlashCommand
    "currtime"
    "Displays the Current time in UTC."
    (do x <- getCurrentTime; return $ "The current time is " <> showT x <> ".")


resetArgCounter :: SlashCommand
resetArgCounter =
  basicSlashCommand
    "resetac"
    "Resets the time since the last autistic argument."
    saveTimeIO
      where
        saveTimeIO :: IO Text
        saveTimeIO = do
          currTime <- getCurrentTime
          setSavedTime _ARGTIMER_FILEPATH currTime
          return "Time since the last autistic argument: 0 days"


viewArgCounter :: SlashCommand
viewArgCounter =
  basicSlashCommand
    "ac"
    "Gives the time since the last autistic argument."
    timeDiffIO
      where
        timeDiffIO = do
          timeDiff <- getTimeDiff _ARGTIMER_FILEPATH
          return $ "Time since the last autistic argument: " <> (T.pack $ formatDiffTime timeDiff) <> "..."


printGiantGlorp :: SlashCommand
printGiantGlorp =
  basicSlashCommand
    "giantglorp"
    "Prints a giant glorp."
    (return . T.pack $ ".\n" ++ giantGlorp)

giantGlorp :: String
giantGlorp = "I⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⢠⣄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄I\n"
          ++ "I⠄⠄⠄⠄⠄⠄⠘⢻⠄⠄⠄⠄⠄⠄⠄⢡⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄I\n"
          ++ "I⠄⠄⠄⠄⠄⠄⠄⠘⡀⠄⠄⠄⠄⠄⠄⠘⡆⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄I\n"
          ++ "I⠄⠄⠄⠄⠄⠄⠄⠄⣇⠄⠄⣀⣀⣀⣤⣄⣷⣤⡀⠄⠄⠄⠄⠄⠄⠄⠄I\n"
          ++ "I⠄⠄⠄⠄⠄⠄⣀⣸⣿⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣧⣠⠄⠄⠄⠄⠄⠄I\n"
          ++ "I⠄⠄⠄⠄⢀⣸⣿⣿⣿⣿⣿⢹⣿⣿⣿⣿⣿⣿⠹⣿⣿⣷⣤⡄⠄⠄⠄I\n"
          ++ "I⠄⠄⠄⠰⣿⣍⡻⣿⠿⠿⠾⢾⣿⣿⣿⣿⣿⣿⠿⠛⠻⢿⡟⠄⠄⠄⠄I\n"
          ++ "I⠄⠄⠄⠄⣿⣏⣰⣿⣦⣀⠄⠄⢹⣿⣿⣿⣿⣏⡀⠄⣀⣿⣿⠄⠄⠄⠄I\n"
          ++ "I⠄⠄⠄⠄⠿⠿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣟⣫⣭⡄⠄⠄⠄I\n"
          ++ "I⠄⠄⠄⠄⠸⠿⠶⠶⢿⣿⣿⣿⣿⣿⣅⣖⣼⣿⣿⣿⣯⣽⣭⠉⠄⠄⠄I\n"
          ++ "I⠄⠄⠄⠄⠄⢸⣿⣿⣿⣿⣿⣿⣿⣿⣭⣤⣽⣿⣿⣿⣿⢿⣻⡇⠄⠄⠄I\n"
          ++ "I⠄⠄⠄⠄⠄⣼⣿⣿⣿⣿⣿⣿⡿⢿⡿⣿⢿⣟⣛⣿⣷⣿⣿⣿⣂⠄⠄I\n"
          ++ "I⠄⠄⠄⠄⣸⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣶⣿⣿⣿⣿⣿⣿⣿⣿⣿⡆⠄I\n"


makePrediction :: SlashCommand
makePrediction = SlashCommand
  { commandName = "predict"
  , commandRegistration = Just reg
  , commandHandler = \conn wake intr _ -> do
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
                savePrediction conn wake predictionData
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


makeWager :: SlashCommand
makeWager = SlashCommand
  { commandName = "wager"
  , commandRegistration = Just reg
  , commandHandler = \conn wake intr _ -> do
      currTime <- liftIO getCurrentTime
      let iTok = interactionToken intr -- save the interaction token to get the message ID of the reply later
      let pcd = parseWagerCommand currTime intr :: Either T.Text WagerCommandData
      let botReply = createInitialWagerResponse currTime pcd

      -- Respond with the error or the correct reply
      void . restCall $
        R.CreateInteractionResponse
          (interactionId intr)
          (interactionToken intr)
          botReply

      -- TODO: Database stuff
  }
    where
      reg = -- Command registration
        CreateApplicationCommandChatInput
          "wager"
          Nothing
          "Offer a wager!"
          Nothing (
            Just (
              OptionsValues
                [
                  OptionValueString
                    "claim"
                    Nothing
                    "What you are betting will happen"
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
                , OptionValueNumber
                    "amount"
                    Nothing
                    "Amount of currency you're betting"
                    Nothing
                    True
                    (Left False)
                    (Just 0)
                    Nothing
                , OptionValueString
                    "odds"
                    Nothing
                    "(Optional) odds you're giving"
                    Nothing
                    False
                    (Left False)
                    (Just 3)
                    Nothing
                ]
              )
            )
          Nothing
          (Just False)

