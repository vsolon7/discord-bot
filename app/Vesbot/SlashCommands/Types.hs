module Vesbot.SlashCommands.Types where
import Discord
import Discord.Internal.Types.ApplicationCommands
import Discord.Internal.Types.Interactions
import Control.Concurrent.MVar
import qualified Data.Text as T

import Vesbot.Database


-- TODO: Create different slash command types? Not all slash commands need access to the database or
-- the MVar used to wake the prediction notifier
data SlashCommand = SlashCommand
  { commandName :: T.Text
  , commandRegistration :: Maybe CreateApplicationCommand
  , commandHandler :: DbConnection
                   -> MVar ()
                   -> Interaction
                   -> Maybe OptionsData
                   -> DiscordHandler ()
  }
