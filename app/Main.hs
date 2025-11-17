{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Discord
import Discord.Types
import Discord.Interactions
import UnliftIO (liftIO)
import Data.List (find)
import Control.Monad (forM_)
import Utils (echo, showT, getToken, getGuildId)
import Data.Text (Text)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import qualified Discord.Requests as R
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (getCurrentTime)
import Commands


-- MAIN

main :: IO ()
main = do
  tok <- getToken
  guildId <- getGuildId

  botTerminationError <- runDiscord $ def
    { discordToken = tok
    , discordOnEvent = onDiscordEvent guildId
    -- If you are using application commands, you might not need
    -- message contents at all
    , discordGatewayIntent = def { gatewayIntentMessageContent = True }
    }

  echo $ "A fatal error occurred: " <> botTerminationError

-- EVENTS

onDiscordEvent :: GuildId -> Event -> DiscordHandler ()
onDiscordEvent guildId = \case
  Ready _ _ _ _ _ _ (PartialApplication appId _) -> onReady appId guildId
  InteractionCreate intr                         -> onInteractionCreate intr
  _                                              -> pure ()


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
  tryRegistering cmd = case registration cmd of
    Just reg -> restCall $ R.CreateGuildApplicationCommand appId guildId reg
    Nothing  -> pure . Left $ RestCallErrorCode 0 "" ""

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
onInteractionCreate :: Interaction -> DiscordHandler ()
onInteractionCreate = \case
  cmd@InteractionApplicationCommand
    { applicationCommandData = input@ApplicationCommandDataChatInput {} } ->
      case
        find (\c -> applicationCommandDataName input == name c) mySlashCommands
      of
        Just found ->
          handler found cmd (optionsData input)

        Nothing ->
          echo "Somehow got unknown slash command (registrations out of date?)"

  _ ->
    pure () -- Unexpected/unsupported interaction type
