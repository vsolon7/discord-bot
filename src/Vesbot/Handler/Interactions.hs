{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Handler.Interactions
  ( interactionHandler
  ) where

import Vesbot.Config (Config)
import Vesbot.Utils (find)
import Vesbot.Database.Types (DBConnection)
import Vesbot.Logging as Logging (echo)
import Vesbot.SlashCommands as SC

import Discord
import Discord.Interactions

-- | Only supports application commands currently. When someone uses an application command, the
-- function tries to look it up in the list of the registered commands.
interactionHandler :: Config -> DBConnection -> Interaction -> DiscordHandler ()
interactionHandler cfg dbconn =
  \intr -> case intr of
    cmd@InteractionApplicationCommand
      { applicationCommandData = input@ApplicationCommandDataChatInput {}
      } ->
        case find (\c -> applicationCommandDataName input == commandName c) (SC.slashCommands dbconn) of
          Just found -> do
            (SC.commandHandler found) cmd (optionsData input)
          Nothing ->
            Logging.echo "Somehow got unknown slash command (registrations out of date?)"
    _ ->
      return () -- Unexpected/unsupported interaction type
