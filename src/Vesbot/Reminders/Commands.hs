{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Reminders.Commands where

import Vesbot.Utils (showT, maybeToEither, ephemeralResponseBasic, void, liftIO)
import Vesbot.Config (Env(..))
import Vesbot.Logging as Logging (log, LogType(..))
import Vesbot.Parsing (extractStringOption, parseDateInput, utcToInt, getUserData)
import Vesbot.Actions (Action(..))
import Vesbot.Reminders.Types
import Vesbot.Reminders.Database
import Vesbot.BotCommands.Types (BotCommand(..))

import JSONScheduler.Scheduler (Schedule(..), scheduleJob)

import Discord
import Discord.Types
import Discord.Internal.Types.Interactions
import Discord.Internal.Types.ApplicationCommands
import qualified Discord.Requests as R
import Data.Time
import qualified Data.Text as T

parseReminderCommand :: UTCTime -> Interaction -> Maybe OptionsData -> Either T.Text (ReminderData, ChannelId)
parseReminderCommand currTime cmd@InteractionApplicationCommand {} opts =
  do
    dataValues <-
      maybeToEither
        "Could not access the reminder command's option fields."
        opts
    reminderText <-
      maybeToEither
        "Could not access content field."
        (extractStringOption "content" dataValues)
    dueDateText <-
      maybeToEither
        "Could not access reminder date field."
        (extractStringOption "date" dataValues)
    _ <-
      maybeToEither
        "Error accessing Guild ID for the remind command. Note that this command can only be used in a server."
        (interactionGuildId cmd)
    cid <-
      maybeToEither
        "Error accessing channel ID for the remind command. Note that this command can only be used in a server."
        (interactionChannelId cmd)
    uid <-
      maybeToEither
        "Could not get user data for the remind command. Note that this command can only be used in a server."
        (getUserData . interactionUser $ cmd)
    dueDateUTC <-
      maybeToEither
        (T.intercalate "\n"
          [ "Error parsing reminder date. Here are the formatting options."
          , "**Relative time:** \"in #w\" or \"in #d\" or \"in #h\" or \"in #m\", where # is a positive integer.\
          \ \"w\" stands for weeks, \"d\" stands for days, \"h\" stands for hours, and \"m\" stands for minutes."
          , "**Absolute time:** \"on month-day-year HH:MM <timezone>\", where the timezone is optional.\
          \If no timezone is entered, the bot will default to UTC. Timezone examples are CST, CDT, EST, etc."
          ]
        )
        (parseDateInput currTime . T.unpack $ dueDateText)
    return
      ( ReminderData
          { reminderUserId = uid
          , reminderContent = reminderText
          , reminderMadeDate = currTime
          , reminderDueDate = dueDateUTC
          }
      , cid
      )

parseReminderCommand _ _ _ = Left "Tried to parse an interaction that is not a chat input slash command when parsing the reminder command."


-- | Generate the bot reply to the command. parseReminderCommand returns either an error message or a record of the
-- relevant reminder command data. We must either respond with an (ephemeral) error, or else form the bot reply
-- in the case that the user actually entered a future time.
createInitialReminderResponse :: UTCTime -> Either T.Text (ReminderData, ChannelId) -> InteractionResponse
createInitialReminderResponse currTime mrcd =
  case mrcd of
    Right (rd,_) ->
      if currTime < reminderDueDate rd then
        let dueDateUTC = showT . utcToInt $ reminderDueDate rd
            dueDateTimeStampRel = "<t:" <> dueDateUTC <> ":R>"
            dueDateTimeStampAbs = "<t:" <> dueDateUTC <> ":f>"
            response =
              "Reminder set! On " <> dueDateTimeStampAbs <> ", which is in " <> dueDateTimeStampRel <>
              ", I will remind you of the following:\n*" <> reminderContent rd <> "*."
        in
          interactionResponseBasic response
      else
        ephemeralResponseBasic "You can only set reminders for times in the future!"
    Left err -> ephemeralResponseBasic err


setReminder :: Env -> BotCommand
setReminder env = SlashCommand
  { commandName = "remind"
  , commandRegistration = Just reg
  , slashCommandHandler = \intr opts -> do
      currTime <- liftIO getCurrentTime
      let iTok = interactionToken intr -- save the interaction token to get the message ID of the bot's reply
      let mrd = parseReminderCommand currTime intr opts :: Either T.Text (ReminderData, ChannelId)
      let botReply = createInitialReminderResponse currTime mrd

      -- Respond with the error or the correct reply
      void . restCall $
        R.CreateInteractionResponse
          (interactionId intr)
          (interactionToken intr)
          botReply

      -- | If the reminderCommandData was parsed correctly, we want to save the reminder to the database
      -- We can't do this before because we also want to save the ID of the message the bot responds to the
      -- slash command with, and this message is sent **after** the restCall immediately above.
      case mrd of
        Right (rd, cid) | currTime < reminderDueDate rd -> do
          maybeResult <- restCall $
            R.GetOriginalInteractionResponse
              (interactionApplicationId intr)
              iTok
          case maybeResult of
            Right msg -> do
              void . liftIO $ do
                rid <- saveReminder (envDBConnection env) rd
                let a = SendReminder rid cid (messageId msg)
                r <- scheduleJob (envScheduler env) (Just $ "reminder:" <> showT rid) (Once . reminderDueDate $ rd) a
                case r of
                  Left err -> Logging.log Error $ "Error scheduling a reminder reply. \
                    \The job scheduler returned the following error:\n" <> err
                  _ -> return ()
            Left err -> Logging.log Error $ "Error acquiring the bot's reminder reply message.\
              \Discord sent the following error message:\n" <> showT err
        _ -> return ()
  }
    where
      reminderField = OptionValueString "content" Nothing "What you want to be reminded of" Nothing True (Left False) (Just 1) Nothing
      dateField = OptionValueString "date" Nothing "\"in #[w|d|h|m]\" or \"on MM-DD-YYYY HH:MM <timezone>\"" Nothing True (Left False) (Just 1) Nothing
      reg =
        CreateApplicationCommandChatInput
          "remind"
          Nothing
          "Create a reminder"
          Nothing
          (Just $ OptionsValues [reminderField, dateField ])
          Nothing
          (Just False)

