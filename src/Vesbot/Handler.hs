{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Handler where

import Vesbot.Utils (showT, forM_)
import Vesbot.Logging as Logging (echo)
import Vesbot.Config as Config (InitialEnv, Config, cfgGuildId, envDBConnection)
import Vesbot.Handler.Interactions (interactionHandler)
import Vesbot.Handler.Messages (messageHandler)
import Vesbot.SlashCommands as SC

import Discord
import Discord.Types
import Discord.Interactions
import qualified Discord.Requests as R


-- This function receives every Discord event and decides what to do with it.
eventHandler :: Config
               -> InitialEnv
               -> Event
               -> DiscordHandler ()
eventHandler cfg env =
  \event ->
    case event of
      Ready _ _ _ _ _ _ (PartialApplication appid _) -> onReady appid (cfgGuildId cfg)
      InteractionCreate intr  -> interactionHandler cfg (envDBConnection env) intr
      MessageCreate mess      -> messageHandler cfg (envDBConnection env) mess
      MessageReactionAdd info -> return ()
      _                       -> return ()


-- Registers the application commands defined in Commands.hs when the bot is ready.
onReady :: ApplicationId -> GuildId -> DiscordHandler ()
onReady appId gId = do
  Logging.echo "Bot ready!"

  appCmdRegistrations <- mapM tryRegistering SC.slashCommands

  case sequence appCmdRegistrations of
    Left err ->
      Logging.echo $ "Failed to register some commands" <> showT err
    Right cmds -> do
      Logging.echo $ "Registered " <> showT (length cmds) <> " command(s)."
      unregisterOutdatedCmds cmds
  where
    tryRegistering cmd = case SC.commandRegistration cmd of
      Just reg -> restCall $ R.CreateGuildApplicationCommand appId gId reg
      Nothing -> return . Left $ RestCallErrorCode 0 "" ""

    -- Unregisters commands that existed on the last iteration of the bot, but no longer exist.
    unregisterOutdatedCmds validCmds = do
      registered <- restCall $ R.GetGuildApplicationCommands appId gId
      case registered of
        Left err ->
          Logging.echo $ "Failed to get registered slash commands: " <> showT err
        Right cmds ->
          let validIds = map applicationCommandId validCmds
              outdatedIds = filter (`notElem` validIds) . map applicationCommandId $ cmds
           in forM_ outdatedIds $ restCall . R.DeleteGuildApplicationCommand appId gId
