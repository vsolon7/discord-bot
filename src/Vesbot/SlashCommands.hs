{-# LANGUAGE OverloadedStrings #-}
module Vesbot.SlashCommands
  ( SlashCommand
  , commandName
  , commandRegistration
  , commandHandler
  , slashCommands
  ) where

import Vesbot.Utils (liftIO, void, showT)
import Vesbot.Database.Types (DBConnection)
import Vesbot.SlashCommands.Types

import Vesbot.ArgumentTimer.Commands (viewAC, resetAC)

import Discord
import Discord.Internal.Types.ApplicationCommands
import Discord.Internal.Types.Interactions
import qualified Discord.Requests as R

import qualified Data.Text as T
import Data.Time (getCurrentTime)
 

-- | Constructor for a basic slash command with no options.
-- it just replies with some text that is the result of running IO actions.
basicSlashCommand :: T.Text    -> -- Slash Command Name
                     T.Text    -> -- Registration Description
                     IO T.Text -> -- Text diplayed in the interaction response, possibly obtained with IO
                     SlashCommand
basicSlashCommand name regDesc statefulText
  = SlashCommand
    { commandName = name
    , commandRegistration = createChatInput name regDesc
    , commandHandler = \intr _ -> do
        iomessage <- liftIO statefulText
        void . restCall $
          R.CreateInteractionResponse
            (interactionId intr)
            (interactionToken intr)
            (interactionResponseBasic iomessage)
    }


slashCommands :: DBConnection -> [SlashCommand]
slashCommands dbconn =
  [ ping
  , time
  , glorp
  , viewAC dbconn
  , resetAC dbconn
  ]


ping :: SlashCommand
ping =
  basicSlashCommand
    "ping"
    "Responds 'pong'"
    (return "pong!")


time :: SlashCommand
time =
  basicSlashCommand
    "time"
    "Displays the Current time in UTC."
    (do x <- getCurrentTime; return $ "The current time is " <> showT x <> ".")


glorp :: SlashCommand
glorp =
  basicSlashCommand
    "glorp"
    "Prints a giant glorp."
    (return $ ".\n" <> giantGlorp)
  where
    giantGlorp :: T.Text
    giantGlorp = "I⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⢠⣄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄I\n"
              <> "I⠄⠄⠄⠄⠄⠄⠘⢻⠄⠄⠄⠄⠄⠄⠄⢡⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄I\n"
              <> "I⠄⠄⠄⠄⠄⠄⠄⠘⡀⠄⠄⠄⠄⠄⠄⠘⡆⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄I\n"
              <> "I⠄⠄⠄⠄⠄⠄⠄⠄⣇⠄⠄⣀⣀⣀⣤⣄⣷⣤⡀⠄⠄⠄⠄⠄⠄⠄⠄I\n"
              <> "I⠄⠄⠄⠄⠄⠄⣀⣸⣿⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣧⣠⠄⠄⠄⠄⠄⠄I\n"
              <> "I⠄⠄⠄⠄⢀⣸⣿⣿⣿⣿⣿⢹⣿⣿⣿⣿⣿⣿⠹⣿⣿⣷⣤⡄⠄⠄⠄I\n"
              <> "I⠄⠄⠄⠰⣿⣍⡻⣿⠿⠿⠾⢾⣿⣿⣿⣿⣿⣿⠿⠛⠻⢿⡟⠄⠄⠄⠄I\n"
              <> "I⠄⠄⠄⠄⣿⣏⣰⣿⣦⣀⠄⠄⢹⣿⣿⣿⣿⣏⡀⠄⣀⣿⣿⠄⠄⠄⠄I\n"
              <> "I⠄⠄⠄⠄⠿⠿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣟⣫⣭⡄⠄⠄⠄I\n"
              <> "I⠄⠄⠄⠄⠸⠿⠶⠶⢿⣿⣿⣿⣿⣿⣅⣖⣼⣿⣿⣿⣯⣽⣭⠉⠄⠄⠄I\n"
              <> "I⠄⠄⠄⠄⠄⢸⣿⣿⣿⣿⣿⣿⣿⣿⣭⣤⣽⣿⣿⣿⣿⢿⣻⡇⠄⠄⠄I\n"
              <> "I⠄⠄⠄⠄⠄⣼⣿⣿⣿⣿⣿⣿⡿⢿⡿⣿⢿⣟⣛⣿⣷⣿⣿⣿⣂⠄⠄I\n"
              <> "I⠄⠄⠄⠄⣸⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣶⣿⣿⣿⣿⣿⣿⣿⣿⣿⡆⠄I\n"
