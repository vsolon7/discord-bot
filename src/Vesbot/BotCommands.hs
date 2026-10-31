{-# LANGUAGE OverloadedStrings #-}
module Vesbot.BotCommands
  ( BotCommand(..)
  , slashCommands
  , messageCommands
  ) where


import Vesbot.Logging as Logging (LogType(..), log)
import Vesbot.Utils (showT, maybeToEither, void, liftIO, ephemeralResponseBasic, forM_, threadDelay, announce)
import Vesbot.Parsing (extractIntegerOption)
import Vesbot.Config (Env(..), Config(..))
import Vesbot.BotCommands.Types
import Vesbot.ArgumentTimer.Commands (viewAC, resetAC)
import Vesbot.Predictions.Commands (makePrediction)
import Vesbot.Currency.Types (EmojiUpdate(..))
import Vesbot.Currency.Database (updateEmojis)
import Vesbot.Currency.Commands (viewCurrencyCommand, payCommand, viewRichestUsersCommand, viewEmojisCommand)
import Vesbot.Reminders.Commands (setReminder)
import Vesbot.Responses (RareEmoji(..), selectNRandomRareEmojis )

import Discord
import Discord.Types
import qualified Discord.Requests as R
import Discord.Internal.Types.ApplicationCommands
import Discord.Internal.Types.Interactions

import qualified Data.Text as T
import Data.Time (getCurrentTime)
import System.Random.Stateful (uniformRM, globalStdGen)
import Control.Monad (when)


-- | Some are slash commands, some are message commands.
slashCommands :: Env -> Config -> [BotCommand]
slashCommands env cfg =
  [ ping
  , time
  , glorp
  , roll cfg env
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


roll :: Config -> Env -> BotCommand
roll cfg env = SlashCommand
  { commandName = "roll"
  , commandRegistration = Just reg
  , slashCommandHandler = \intr opts -> do
      let intOrErr = parseCommand intr opts
      case intOrErr of
        Left err -> do
          void . restCall $
            R.CreateInteractionResponse
              (interactionId intr)
              (interactionToken intr)
              (ephemeralResponseBasic err) 
        Right n -> do
          rand <- liftIO $ uniformRM (1, n) globalStdGen :: DiscordHandler Integer
          let iTok = interactionToken intr
              response = "You rolled a " <> showT rand <> " on the " <> "d" <> showT n <> "."
          void . restCall $
            R.CreateInteractionResponse
              (interactionId intr)
              (interactionToken intr)
              (interactionResponseBasic response)

          Logging.log Info $ "User rolled a " <> "d" <> showT n <> " with the roll command and rolled a " <> showT rand <> "."

          -- Lots of error checking...
          when (n >= 500 && rand == n) $ do -- if you're rolling at least a d500 and you roll a max, you get a prize
            maybeMsg <- restCall $ -- get the message the bot responded with.
              R.GetOriginalInteractionResponse
              (interactionApplicationId intr)
              iTok
            case maybeMsg of
              Left err ->
                Logging.log Error $
                  "Failed to respond to a high roller. Discord sent the following error message:\n" <> showT err
              Right msg -> do
                -- we only want to do something if the user is a guild member
                case interactionUser intr of
                  MemberOrUser (Right _) ->
                    liftIO $ Logging.log Warn $
                      "Was going to give reactions to a high roller, but they are not guild member! The bot is only\
                      \ designed to be used in a server."
                  MemberOrUser (Left gm) ->
                    case memberUser gm of
                      Nothing -> Logging.log Error $
                        "Was going to give reactions to a high roller, but could not obtain their user data."
                      Just u -> do
                        -- react with some rare emojis
                        reacts <- liftIO $ selectNRandomRareEmojis (fromInteger (n `div` 200)) (cfgRareEmoji cfg)
                        forM_ reacts $ \r -> do
                          rareEmojiHandler r msg
                          threadDelay 250000 -- wait 0.25 seconds in between reacts

                        let uid = userId u
                            userPing = "<@" <> showT uid <> ">"
                            eUpdate = EmojiUpdate uid [(rareEmojiId re, 1) | re <- reacts ]
                            cid = showT . messageChannelId $ msg
                            mid = showT . messageId $ msg
                            link = "https://discord.com/channels/" <> (showT . cfgGuildId $ cfg) <> "/" <> cid <> "/" <> mid
                            message = "HOLY. " <> userPing <> " just rolled a " <> "d" <> showT n <> " and rolled the higest value. CHECK IT: " <> link
                        liftIO $ updateEmojis (envDBConnection env) eUpdate
                        announce (cfgAnnouncementCID cfg) message

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

