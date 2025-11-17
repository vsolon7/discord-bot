{-# LANGUAGE OverloadedStrings #-}
module Commands where

import Discord
import Discord.Interactions
import UnliftIO (liftIO)
import Data.Text (Text)
import Control.Monad (void)
import qualified Discord.Requests as R
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Utils

data SlashCommand = SlashCommand
  { name :: Text
  , registration :: Maybe CreateApplicationCommand
  , handler :: Interaction -> Maybe OptionsData -> DiscordHandler ()
  }

_arg_time_filePath = "appdata/savedtime"

mySlashCommands :: [SlashCommand]
mySlashCommands = [ping, getCurrTime, resetArgCounter, viewArgCounter, printGiantGlorp]

ping :: SlashCommand
ping = SlashCommand
  { name = "ping"
  , registration = createChatInput "ping" "Responds 'pong'"
  , handler = \intr _options ->
      void . restCall $
        R.CreateInteractionResponse
          (interactionId intr)
          (interactionToken intr)
          (interactionResponseBasic "pong")
  }

getCurrTime :: SlashCommand
getCurrTime = SlashCommand
  { name = "curtime",
    registration = createChatInput "curtime" "Displays the current time in UTC",
    handler = \intr _options -> do
      currTime <- liftIO $ getCurrentTime
      let message = T.pack $ "The current time is " ++ show currTime
      void . restCall $
        R.CreateInteractionResponse
          (interactionId intr)
          (interactionToken intr)
          (interactionResponseBasic message)
  }

resetArgCounter :: SlashCommand
resetArgCounter = SlashCommand
  { name = "resetac",
    registration = createChatInput "resetac" "Resets the time since the last autistic argument",
    handler = \intr _options -> do
      currTime <- liftIO $ getCurrentTime
      liftIO $ setSavedTime _arg_time_filePath currTime
      let message = T.pack "Time since last autistic argument: 0 days."
      void . restCall $
        R.CreateInteractionResponse
          (interactionId intr)
          (interactionToken intr)
          (interactionResponseBasic message)
  }

viewArgCounter :: SlashCommand
viewArgCounter = SlashCommand
  { name = "ac",
    registration = createChatInput "ac" "Gives the time since the last autistic argument",
    handler = \intr _options -> do
      timeDiff <- liftIO $ getTimeDiff _arg_time_filePath
      let message = T.pack $ "Time since last autistic argument: " ++ (formatDiffTime timeDiff) ++ "."
      void . restCall $
        R.CreateInteractionResponse
          (interactionId intr)
          (interactionToken intr)
          (interactionResponseBasic message)
  }

printGiantGlorp :: SlashCommand
printGiantGlorp = SlashCommand
  { name = "giantglorp",
    registration = createChatInput "giantglorp" "prints a giant glorp",
    handler = \intr _options ->
      void . restCall $
        R.CreateInteractionResponse
          (interactionId intr)
          (interactionToken intr)
          (interactionResponseBasic (T.pack $ ".\n" ++ giantGlorp))
  }

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
