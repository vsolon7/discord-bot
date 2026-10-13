{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Handler.Interactions
  ( interactionHandler
  ) where

import Vesbot.Config (Config(..), Env(..))
import Vesbot.Utils (find)
import Vesbot.Database (DBConnection)
import Vesbot.Logging as Logging (echo)
import Vesbot.SlashCommands as SC

import Discord
import Discord.Interactions

-- | Only supports application commands currently. When someone uses an application command, the
-- function tries to look it up in the list of the registered commands.
interactionHandler :: Config -> Env -> Interaction -> DiscordHandler ()
interactionHandler cfg env =
  \intr -> case intr of
    cmd@InteractionApplicationCommand
      { applicationCommandData = input@ApplicationCommandDataChatInput {}
      } ->
        case find (\c -> applicationCommandDataName input == commandName c) (SC.slashCommands env) of
          Just found -> do
            (SC.commandHandler found) cmd (optionsData input)
          Nothing ->
            Logging.echo "Somehow got unknown slash command (registrations out of date?)"
    _ ->
      return () -- Unexpected/unsupported interaction type
