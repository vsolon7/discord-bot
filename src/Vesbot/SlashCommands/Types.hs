module Vesbot.SlashCommands.Types where

import Discord
import Discord.Internal.Types.ApplicationCommands
import Discord.Internal.Types.Interactions

import qualified Data.Text as T


data SlashCommand = SlashCommand
  { commandName :: T.Text
  , commandRegistration :: Maybe CreateApplicationCommand
  , commandHandler :: Interaction
                   -> Maybe OptionsData
                   -> DiscordHandler ()
  }
