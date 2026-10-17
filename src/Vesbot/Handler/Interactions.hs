{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Handler.Interactions
  ( interactionHandler
  ) where

import Vesbot.Config (Config(..), Env(..))
import Vesbot.Utils (find)
import Vesbot.Logging as Logging (log, LogType(..))
import Vesbot.Database (DBConnection)
import Vesbot.BotCommands as BC

import Discord
import Discord.Interactions

-- | Only supports application commands currently. When someone uses an application command, the
-- function tries to look it up in the list of the registered commands.
interactionHandler :: Config -> Env -> Interaction -> DiscordHandler ()
interactionHandler cfg env =
  \intr -> case intr of
    cmd@InteractionApplicationCommand {} -> case applicationCommandData cmd of
      slashInput@ApplicationCommandDataChatInput {} ->
        case find (\c -> applicationCommandDataName slashInput == commandName c) (BC.slashCommands env cfg) of
          Just found -> do
            (BC.slashCommandHandler found) cmd (optionsData slashInput)
          Nothing ->
            Logging.log Warn $ "Somehow got unknown slash command (registrations out of date?)"
      msgInput@ApplicationCommandDataMessage {} ->
        case find (\c -> applicationCommandDataName msgInput == commandName c) (BC.messageCommands env) of
          Just found -> do
            (BC.messageCommandHandler found) cmd
          Nothing ->
            Logging.log Warn $ "Somehow got unknown message command (registrations out of date?)"
    i@InteractionComponent { componentData = b@ButtonData {} } -> buttonInteractionHandler i
    _ -> return () -- Unexpected/unsupported interaction type


buttonInteractionHandler :: Interaction -> DiscordHandler ()
buttonInteractionHandler i@InteractionComponent {} = return ()
