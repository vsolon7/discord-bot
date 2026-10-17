module Vesbot.BotCommands.Types
  ( BotCommand(..)
  , basicSlashCommand
  ) where

import Vesbot.Utils (liftIO, void)

import Discord
import Discord.Internal.Types.ApplicationCommands
import Discord.Internal.Types.Interactions
import qualified Discord.Requests as R

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

-- | Constructor for a basic slash command with no options.
-- it just replies with some text that is the result of running IO actions.
basicSlashCommand :: T.Text    -> -- Slash Command Name
                     T.Text    -> -- Registration Description
                     IO T.Text -> -- Text diplayed in the interaction response, possibly obtained with IO
                     BotCommand
basicSlashCommand name regDesc statefulText
  = SlashCommand
    { commandName = name
    , commandRegistration = createChatInput name regDesc --createChatInput is from Discord.Internal.Types.ApplicationCommands
    , slashCommandHandler = \intr _ -> do
        iomessage <- liftIO statefulText
        void . restCall $
          R.CreateInteractionResponse
            (interactionId intr)
            (interactionToken intr)
            (interactionResponseBasic iomessage)
    }
