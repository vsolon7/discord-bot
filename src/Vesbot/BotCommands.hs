{-# LANGUAGE OverloadedStrings #-}
module Vesbot.BotCommands
  ( BotCommand(..)
  , slashCommands
  , messageCommands
  ) where


import Vesbot.Utils (showT, maybeToEither, void, liftIO, ephemeralResponseBasic)
import Vesbot.Parsing (extractIntegerOption)
import Vesbot.Config (Env(..), Config(..))
import Vesbot.BotCommands.Types
import Vesbot.ArgumentTimer.Commands (viewAC, resetAC)
import Vesbot.Predictions.Commands (makePrediction)
import Vesbot.Currency.Commands (viewCurrencyCommand, payCommand, viewRichestUsersCommand, viewEmojisCommand)
import Vesbot.Reminders.Commands (setReminder)

import Discord
import qualified Discord.Requests as R
import Discord.Internal.Types.ApplicationCommands
import Discord.Internal.Types.Interactions

import qualified Data.Text as T
import Data.Time (getCurrentTime)
import System.Random.Stateful (uniformRM, globalStdGen)


-- | Some are slash commands, some are message commands.
slashCommands :: Env -> Config -> [BotCommand]
slashCommands env cfg =
  [ ping
  , time
  , glorp
  , roll
  , viewAC (envDBConnection env) cfg
  , makePrediction env
  , setReminder env
  , viewCurrencyCommand (envDBConnection env) cfg
  , payCommand (envDBConnection env) cfg
  , viewRichestUsersCommand (envDBConnection env) cfg
  , viewEmojisCommand (envDBConnection env)
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


roll :: BotCommand
roll = SlashCommand
  { commandName = "roll"
  , commandRegistration = Just reg
  , slashCommandHandler = \intr opts -> do
      let mn = parseCommand intr opts
      botReply <- liftIO $ createResponse mn

      void . restCall $
        R.CreateInteractionResponse
          (interactionId intr)
          (interactionToken intr)
          botReply
  }
  where
    opt = OptionValueInteger "n" Nothing "The number of sides the dice has" Nothing True (Left False) (Just 1) Nothing
    reg = -- Command registration
      CreateApplicationCommandChatInput
        "roll"
        Nothing
        ("Roll a dice with n sides")
        Nothing 
        (Just $ OptionsValues [opt])
        Nothing
        (Just False)

    parseCommand :: Interaction -> Maybe OptionsData -> Either T.Text Integer
    parseCommand (InteractionApplicationCommand {}) opts = do
      dataValues <-
        maybeToEither
          "Could not access the roll command's option fields."
          opts
      maybeToEither
          "Could not access n on the roll command option fields."
          (extractIntegerOption "n" dataValues)
    parseCommand _ _ = Left "Tried to parse a non slash command on an interaction that is not an application command."

    createResponse :: Either T.Text Integer -> IO InteractionResponse
    createResponse intOrErr = do
      case intOrErr of
        Left err -> return (ephemeralResponseBasic err)
        Right n -> do
          rand <- uniformRM (1, n) globalStdGen :: IO Integer
          let response = "You rolled a " <> showT rand <> " on the " <> "d" <> showT n <> "."
          return (interactionResponseBasic response)
