module Vesbot.BotCommands.Types
  ( BotCommand(..)
  ) where

import Discord
import Discord.Internal.Types.ApplicationCommands
import Discord.Internal.Types.Interactions

import qualified Data.Text as T

data BotCommand
  = SlashCommand
      { commandName :: T.Text
      , commandRegistration :: Maybe CreateApplicationCommand
      , slashCommandHandler :: Interaction -> Maybe OptionsData -> DiscordHandler ()
      }
  | MessageCommand
      { commandName :: T.Text
      , commandRegistration :: Maybe CreateApplicationCommand
      , messageCommandHandler :: Interaction -> DiscordHandler ()
      }

