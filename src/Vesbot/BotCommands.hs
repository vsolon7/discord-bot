{-# LANGUAGE OverloadedStrings #-}
module Vesbot.BotCommands
  ( BotCommand(..)
  , slashCommands
  , messageCommands
  ) where

import Vesbot.Utils (liftIO, void, showT)
import Vesbot.Config (Env(..), Config(..))
import Vesbot.Database (DBConnection)
import Vesbot.BotCommands.Types

import Vesbot.ArgumentTimer.Commands (viewAC, resetAC)
import Vesbot.Predictions.Commands (makePrediction)
import Vesbot.Currency.Commands (viewCurrencyCommand, payCommand, viewRichestUsersCommand)

import Discord
import Discord.Internal.Types.ApplicationCommands
import Discord.Internal.Types.Interactions
import qualified Discord.Requests as R

import qualified Data.Text as T
import Data.Time (getCurrentTime)


-- | Some are slash commands, some are message commands.
slashCommands :: Env -> Config -> [BotCommand]
slashCommands env cfg =
  [ ping
  , time
  , glorp
  , viewAC (envDBConnection env) cfg
  , makePrediction env
  , viewCurrencyCommand (envDBConnection env) cfg
  , payCommand (envDBConnection env) cfg
  , viewRichestUsersCommand (envDBConnection env) cfg
  ]

messageCommands :: Env -> [BotCommand]
messageCommands env =
  [ resetAC (envDBConnection env)
  ]


ping :: BotCommand
ping =
  basicSlashCommand
    "ping"
    "Responds 'pong' if the bot is alive"
    (return "pong!")


time :: BotCommand
time =
  basicSlashCommand
    "time"
    "Displays the Current time in UTC"
    (do x <- getCurrentTime; return $ "The current time is " <> showT x <> ".")


glorp :: BotCommand
glorp =
  basicSlashCommand
    "glorp"
    "Prints a giant glorp"
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
