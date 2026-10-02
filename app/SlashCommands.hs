{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module SlashCommands where

import Discord
import Discord.Interactions
import Discord.Internal.Types.Components
import Discord.Types (messageChannelId, messageId, Message)
import UnliftIO (liftIO)
import UnliftIO.Concurrent
import Data.Text (Text)
import Control.Monad (void)
import qualified Discord.Requests as R
import qualified Data.Text as T
import qualified Data.Aeson as AE
import Data.Time (getCurrentTime, UTCTime)
import Utils
import Data.Either (fromRight)
import ArgumentTimer (setSavedTime, getTimeDiff, formatDiffTime)
import GHC.Generics (Generic)

data PredictionData = PredictionData
  { predictionContent :: Text
  , predictionDate :: UTCTime
  } deriving (Generic, Show)

instance AE.FromJSON PredictionData
instance AE.ToJSON PredictionData

data SlashCommand = SlashCommand
  { commandName :: Text
  , commandRegistration :: Maybe CreateApplicationCommand
  , commandHandler :: Interaction -> Maybe OptionsData -> DiscordHandler ()
  }

-- A basic slash command with no options, it just replies with some text, possibly obtained from IO.
basicSlashCommand :: Text    -> -- Slash Command Name
                     Text    -> -- Registration Description
                     IO Text -> -- Text diplayed in the interaction response, possibly obtained with IO
                     SlashCommand

basicSlashCommand name regDesc statefulText
  = SlashCommand
    { commandName = name
    , commandRegistration = createChatInput name regDesc
    , commandHandler = \intr _options -> do
        iomessage <- liftIO $ statefulText
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
  , commandHandler = \intr maybe_options -> do
     let output = case maybe_options of
           Just (OptionsDataValues [pred, date]) -> "Prediction: " <> (fromRight "" . optionDataValueString $ pred) <> "\n\nDate: " <> (fromRight "" . optionDataValueString $ date)
           _ -> "Command Error."
     
     void . restCall $
          R.CreateInteractionResponse
            (interactionId intr)
            (interactionToken intr)
            (interactionResponseBasic output)
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

