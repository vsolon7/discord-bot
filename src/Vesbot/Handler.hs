{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Handler where

import Vesbot.Utils (showT, forM_)
import Vesbot.Logging as Logging (log, LogType(..))
import Vesbot.Config as Config (Env(..), Config(..))
import Vesbot.Database.Types (DBConnection)
import Vesbot.Handler.Interactions (interactionHandler)
import Vesbot.Handler.Messages (messageHandler)
import Vesbot.Handler.Reactions (reactionHandler)
import Vesbot.SlashCommands as SC

import Discord
import Discord.Types
import Discord.Interactions
import qualified Discord.Requests as R


-- This function receives every Discord event and decides what to do with it.
eventHandler :: Config
               -> Env
               -> Event
               -> DiscordHandler ()
eventHandler cfg env =
  \event ->
    case event of
      Ready _ _ _ _ _ _ (PartialApplication appid _) -> onReady env cfg appid (cfgGuildId cfg)
      InteractionCreate intr  -> interactionHandler cfg env intr
      MessageCreate mess      -> messageHandler cfg env mess
      MessageReactionAdd info -> reactionHandler cfg env info
      _                       -> return ()


-- Registers the application commands defined in Commands.hs when the bot is ready.
onReady :: Env -> Config -> ApplicationId -> GuildId -> DiscordHandler ()
onReady env cfg appId gId = do
  Logging.log Always $ "Bot ready!"

  appCmdRegistrations <- mapM tryRegistering (SC.slashCommands env cfg)

  case sequence appCmdRegistrations of
    Left err ->
      Logging.log Error $ "Failed to register some commands" <> showT err
    Right cmds -> do
      Logging.log Info $ "Registered " <> showT (length cmds) <> " command(s)."
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
          Logging.log Error $ "Failed to get registered slash commands: " <> showT err
        Right cmds ->
          let validIds = map applicationCommandId validCmds
              outdatedIds = filter (`notElem` validIds) . map applicationCommandId $ cmds
           in forM_ outdatedIds $ restCall . R.DeleteGuildApplicationCommand appId gId
