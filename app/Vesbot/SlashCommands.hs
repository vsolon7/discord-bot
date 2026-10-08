{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Vesbot.SlashCommands (
  module Vesbot.SlashCommands,
  module Vesbot.SlashCommands.Types
) where

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

import Vesbot.SlashCommands.Types
import Vesbot.Utils
import Vesbot.ArgumentTimer (setSavedTime, getTimeDiff, formatDiffTime)
import Vesbot.Predictions
import Vesbot.Wagers
import Vesbot.Currency


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
mySlashCommands = [ ping, getCurrTime, resetArgCounter, viewArgCounter, printGiantGlorp, makePrediction, makeWager, payCommand, viewCurrency ]


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
