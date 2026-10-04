{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module SlashCommands where

import Discord
import Discord.Interactions
import UnliftIO (liftIO)
import Data.Text (Text)
import Control.Monad (void)
import Control.Concurrent (forkIO)
import qualified Discord.Requests as R
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQL
import Data.Time (getCurrentTime)
import Utils
import ArgumentTimer (setSavedTime, getTimeDiff, formatDiffTime)
import Predictions

data SlashCommand = SlashCommand
  { commandName :: Text
  , commandRegistration :: Maybe CreateApplicationCommand
  , commandHandler :: SQL.Connection -> Interaction -> Maybe OptionsData -> DiscordHandler ()
  }

-- | Constructor for a basic slash command with no options.
-- it just replies with some text, where the text can run IO actions (possibly to obtain it)
basicSlashCommand :: Text    -> -- Slash Command Name
                     Text    -> -- Registration Description
                     IO Text -> -- Text diplayed in the interaction response, possibly obtained with IO
                     SlashCommand

basicSlashCommand name regDesc statefulText
  = SlashCommand
    { commandName = name
    , commandRegistration = createChatInput name regDesc
    , commandHandler = \_ intr _ -> do
        iomessage <- liftIO statefulText
        void . restCall $
          R.CreateInteractionResponse
            (interactionId intr)
            (interactionToken intr)
            (interactionResponseBasic iomessage)
    }

-- List of slash commands to register
mySlashCommands :: [SlashCommand]
mySlashCommands = [ping, getCurrTime, resetArgCounter, viewArgCounter, printGiantGlorp, addPrediction]

ping :: SlashCommand
ping = basicSlashCommand
         ("ping")
         ("Responds 'pong'")
         (return "pong!")


getCurrTime :: SlashCommand
getCurrTime = basicSlashCommand
                ("currtime")
                ("Displays the Current time in UTC.")
                (do x <- getCurrentTime; return $ "The current time is " <> showT x <> ".")


resetArgCounter :: SlashCommand
resetArgCounter = basicSlashCommand
                    ("resetac")
                    ("Resets the time since the last autistic argument.")
                    (saveTimeIO)
  where
    saveTimeIO :: IO Text
    saveTimeIO = do
      currTime <- getCurrentTime
      setSavedTime _ARGTIMER_FILEPATH currTime
      return "Time since the last autistic argument: 0 days"


viewArgCounter :: SlashCommand
viewArgCounter = basicSlashCommand
                   ("ac")
                   ("Gives the time since the last autistic argument.")
                   (timeDiffIO)
  where
    timeDiffIO = do
      timeDiff <- getTimeDiff _ARGTIMER_FILEPATH
      return $ "Time since the last autistic argument: " <> (T.pack $ formatDiffTime timeDiff) <> "..."


printGiantGlorp :: SlashCommand
printGiantGlorp = basicSlashCommand
                    ("giantglorp")
                    ("Prints a giant glorp.")
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

addPrediction :: SlashCommand
addPrediction = SlashCommand
  { commandName = "addprediction"
  , commandRegistration = Just reg
  , commandHandler = \conn intr _ -> do
     currTime <- liftIO getCurrentTime
     let predictionData = parsePrediction currTime intr
     output <- case predictionData of
           Right p ->
             let response = predictionUserName p <> " has made a prediction!\n" <>
                         "Prediction: " <> predictionContent p <> "\n" <>
                         "Date: " <> showT (predictionDueDate p) <> "."
             in do
                  _ <- liftIO . forkIO $ savePrediction conn p
                  return response
           Left err -> return err

     x <- restCall $
          R.CreateInteractionResponse
            (interactionId intr)
            (interactionToken intr)
            (interactionResponseBasic output)
     echo $ showT x
  }
    where
      reg =
        CreateApplicationCommandChatInput
          "addprediction"
          Nothing
          "Predict the future!"
          Nothing
          (
            Just (
              OptionsValues
                [
                  OptionValueString "prediction" Nothing "What you are predicting will happen" Nothing True (Left False) (Just 1) Nothing,
                  OptionValueString "date" Nothing "When you predict it will happen" Nothing True (Left False) (Just 1) Nothing
                ]
            )
          )
          Nothing
          (Just False)

