{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent.MVar
import Control.Monad (forM_)
import Control.Monad.Reader (ask)
import Data.List (find)
import qualified Database.SQLite.Simple as SQL
import Discord
import Discord.Interactions
import qualified Discord.Requests as R
import Discord.Types
import Predictions (DbConnection, initPredictionTable, startNotifier, withDb)
import Responses
import SlashCommands
import UnliftIO (liftIO)
import Utils
import Utils ()

main :: IO ()
main = do
  tok <- getToken -- see Utils.hs
  gId <- getGuildId -- see Utils.hs
  keywordResponseData <- parseJSON _KEYWORD_RESPONSE_FILEPATH :: IO (Maybe [KeywordResponseData])
  let keywordResponses = case keywordResponseData of
        Nothing -> []
        Just res -> map createKeywordResponse res

  conn <- SQL.open "appdata/database/data.db"
  threadSafeDBConn <- newMVar conn -- used to ensure only one thread can access the database at a time
  wake <- newEmptyMVar :: IO (MVar ()) -- used for waking up the prediction checker
  initPredictionTable conn -- see Predictions.hs. Creates the predictions table if it doesn't exist

  -- \| Starts the discord bot.
  -- 1. discordOnStart: When the bot starts, we make a new thread that periodically checks the database for the next due
  -- prediction and sends a reply when it occurs. This requires us to smuggle out the discord handle so that our new
  -- thread has access to the connection. Future functionality might result in smuggling the handle out to more threads.
  -- 2. discordOnEvent: The bot continuously listens for new discord events. Every event it receives, it passes to the
  -- onDiscordEvent function. This function takes in the database connection and the wake MVar because some of the
  -- events (i.e., application commands) will result in writing to the database and/or waking up the prediction
  -- notifier (when a new prediction is added).
  -- 3. discordOnEnd: When the bot is terminated, we close the database connection.
  botTerminationError <-
    runDiscord $
      def
        { discordToken = tok
        , discordOnStart = do
            h <- ask -- smuggle the connection out!
            liftIO (startNotifier threadSafeDBConn wake h)
        , discordOnEvent = onDiscordEvent threadSafeDBConn wake keywordResponses gId
        , discordOnEnd = do
            echo "Bot has disconnected. Cleaning up..."
            withDb threadSafeDBConn (\c -> SQL.close c)
        , discordGatewayIntent = def {gatewayIntentMessageContent = True}
        }

  echo $ "A fatal error occurred: " <> botTerminationError


-- | This function receives every Discord event and decides what to do with it.
onDiscordEvent :: DbConnection -- some bot interaction responses involve database reads/writes
               -> MVar () -- used to the prediction notifier when a new prediction is made
               -> [KeywordResponse]
               -> GuildId
               -> Event
               -> DiscordHandler ()
onDiscordEvent dbconn wake resList gId = \case
  Ready _ _ _ _ _ _ (PartialApplication appId _) -> onReady appId gId
  InteractionCreate intr -> onInteractionCreate dbconn wake intr
  MessageCreate mess -> onMessageCreate resList mess
  _ -> return ()

-- Registers the application commands defined in Commands.hs when the bot is ready.
onReady :: ApplicationId -> GuildId -> DiscordHandler ()
onReady appId gId = do
  echo "Bot ready!"

  -- mySlashCommands comes from SlashCommands.hs
  appCmdRegistrations <- mapM tryRegistering mySlashCommands

  case sequence appCmdRegistrations of
    Left err ->
      echo $ "Failed to register some commands" <> showT err
    Right cmds -> do
      echo $ "Registered " <> showT (length cmds) <> " command(s)."
      unregisterOutdatedCmds cmds
  where
    tryRegistering cmd = case commandRegistration cmd of
      Just reg -> restCall $ R.CreateGuildApplicationCommand appId gId reg
      Nothing -> return . Left $ RestCallErrorCode 0 "" ""

    -- Unregisters commands that existed on the last iteration of the bot, but no longer exist.
    unregisterOutdatedCmds validCmds = do
      registered <- restCall $ R.GetGuildApplicationCommands appId gId
      case registered of
        Left err ->
          echo $ "Failed to get registered slash commands: " <> showT err
        Right cmds ->
          let validIds = map applicationCommandId validCmds
              outdatedIds =
                filter (`notElem` validIds)
                  . map applicationCommandId
                  $ cmds
           in forM_ outdatedIds $
                restCall . R.DeleteGuildApplicationCommand appId gId


-- | Only supports application commands currently. When someone uses an application command, the
-- function tries to look it up in the list of the registered commands.
-- Some application commands write to a database or wake up the prediction notifier.
onInteractionCreate :: DbConnection -> MVar () -> Interaction -> DiscordHandler ()
onInteractionCreate dbconn wake = \case
  cmd@InteractionApplicationCommand
    { applicationCommandData = input@ApplicationCommandDataChatInput {}
    } ->
      case find (\c -> applicationCommandDataName input == commandName c) mySlashCommands of
        Just found -> do
          commandHandler found dbconn wake cmd (optionsData input)
        Nothing ->
          echo "Somehow got unknown slash command (registrations out of date?)"
  _ ->
    return () -- Unexpected/unsupported interaction type


-- | When a message is created, check if it begins with one of the KeywordResponse keywords
onMessageCreate :: [KeywordResponse] -> Message -> DiscordHandler ()
onMessageCreate resList mess = case (fromBot mess) of
  True -> return ()
  _ ->
    case find (\res -> mess `startsWith` (responseKeyword . responseData $ res)) resList of
      Just found ->
        responseHandler found mess
      _ -> return ()
