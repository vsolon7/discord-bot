{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Discord
import Discord.Types
import Discord.Interactions
import Data.List (find)
import UnliftIO (liftIO)
import Control.Monad (forM_)
import Utils (startsWith, echo, showT, getToken, getGuildId, parseJSONResponses)
import qualified Discord.Requests as R
import qualified Data.Aeson as A
import Commands
import Utils


-- Main function.
-- getToken and getGuildId are in Utils.hs
main :: IO ()
main = do
  tok <- getToken
  guildId <- getGuildId
  keyResJSONData <- parseJSONResponses _KEYWORD_RESPONSE_FILE_PATH

  botTerminationError <- runDiscord $ def
    { discordToken = tok
    , discordOnEvent = onDiscordEvent keyResJSONData guildId
    , discordGatewayIntent = def { gatewayIntentMessageContent = True }
    }

  echo $ "A fatal error occurred: " <> botTerminationError

-- EVENTS

onDiscordEvent :: [KeywordResponse] -> GuildId -> Event -> DiscordHandler ()
onDiscordEvent resList guildId = \case
  Ready _ _ _ _ _ _ (PartialApplication appId _) -> onReady appId guildId
  InteractionCreate intr                         -> onInteractionCreate intr
  MessageCreate     mess                         -> onMessageCreate resList mess
  _                                              -> pure ()


-- Registers the application commands defined in Commands.hs when the bot is ready.
onReady :: ApplicationId -> GuildId -> DiscordHandler ()
onReady appId guildId = do
  echo "Bot ready!"
  
  -- mySlashCommands comes from Commands.hs
  appCmdRegistrations <- mapM tryRegistering mySlashCommands

  case sequence appCmdRegistrations of
    Left err ->
      echo $ "[!] Failed to register some commands" <> showT err

    Right cmds -> do
      echo $ "Registered " <> showT (length cmds) <> " command(s)."
      unregisterOutdatedCmds cmds

  where
  tryRegistering cmd = case commandRegistration cmd of
    Just reg -> restCall $ R.CreateGuildApplicationCommand appId guildId reg
    Nothing  -> pure . Left $ RestCallErrorCode 0 "" ""

  -- Unregisters commands that existed on the last iteration of the bot, but no longer exist.
  unregisterOutdatedCmds validCmds = do
    registered <- restCall $ R.GetGuildApplicationCommands appId guildId
    case registered of
      Left err ->
        echo $ "Failed to get registered slash commands: " <> showT err

      Right cmds ->
        let validIds    = map applicationCommandId validCmds
            outdatedIds = filter (`notElem` validIds)
                        . map applicationCommandId
                        $ cmds
         in forM_ outdatedIds $
              restCall . R.DeleteGuildApplicationCommand appId guildId

-- see Commands.hs for mySlashCommands
-- Only supports application commands currently. When someone uses an application command, the function tries to look
-- it up in the list of the registered commands.
onInteractionCreate :: Interaction -> DiscordHandler ()
onInteractionCreate = \case
  cmd@InteractionApplicationCommand
    { applicationCommandData = input@ApplicationCommandDataChatInput {} } ->
      case
        find (\c -> applicationCommandDataName input == commandName c) mySlashCommands
      of
        Just found -> do
          commandHandler found cmd (optionsData input)

        Nothing ->
          echo "Somehow got unknown slash command (registrations out of date?)"
  _ ->
    pure () -- Unexpected/unsupported interaction type

onMessageCreate :: [KeywordResponse] -> Message -> DiscordHandler ()
onMessageCreate resList mess = case (fromBot mess) of
  True -> pure ()
  _    ->
    case
      find (\res -> mess `startsWith` (responseKeyword res)) resList
    of
      Just found ->
        responseHandler found mess
      _          -> pure ()
-------
