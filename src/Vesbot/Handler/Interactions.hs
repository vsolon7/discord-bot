{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Vesbot.Handler.Interactions
  ( interactionHandler
  ) where

import Vesbot.Config (Config(..), Env(..))
import Vesbot.Utils (find, showT)
import Vesbot.Parsing (getUserData)
import Vesbot.Logging as Logging (log, LogType(..))
import Vesbot.BotCommands as BC
import Vesbot.Predictions (predictionButtonResponse)
import Discord
import Discord.Types
import Discord.Interactions

import qualified Data.Text as T
import Data.Int (Int64)

-- | Only supports application commands currently. When someone uses an application command, the
-- function tries to look it up in the list of the registered commands.
interactionHandler :: Config -> Env -> Interaction -> DiscordHandler ()
interactionHandler cfg env =
  \case
    cmd@InteractionApplicationCommand {} -> case applicationCommandData cmd of
      slashInput@ApplicationCommandDataChatInput {} ->
        case find (\c -> applicationCommandDataName slashInput == commandName c) (BC.slashCommands env cfg) of
          Just found -> do
            BC.slashCommandHandler found cmd (optionsData slashInput)
          Nothing ->
            Logging.log Warn $
              "Slash command " <> (showT . applicationCommandDataName $ slashInput) <> " is unknown (registrations out of date?)"
      msgInput@ApplicationCommandDataMessage {} ->
        case find (\c -> applicationCommandDataName msgInput == commandName c) (BC.messageCommands env) of
          Just found -> do
            BC.messageCommandHandler found cmd
          Nothing ->
            Logging.log Warn $
              "Message command " <> (showT . applicationCommandDataName $ msgInput) <> " is unknown (registrations out of date?)"
      _ -> return () -- unsupported ApplicationCommand type
    i@InteractionComponent { componentData = ButtonData {} } -> buttonInteractionHandler env i
    _ -> return () -- Unexpected/unsupported interaction type

--TODO: Really needs a more advanced parsing system for more button types.
buttonInteractionHandler :: Env -> Interaction -> DiscordHandler ()
buttonInteractionHandler env =
  \case
    InteractionComponent
      { componentData = bdata@ButtonData {}
      , interactionId = iid
      , interactionToken = itok
      , interactionMessage = msg
      , interactionUser = mou
      } ->
        case T.splitOn ":" . componentDataCustomId $ bdata of
          ["prediction", pid, torf] -> do
            let muid = getUserData mou
            case muid of
              Nothing ->
                Logging.log Warn $
                  "Could not get the user ID of a user that clicked a prediction result button. \
                  \Note that this bot is desined to be used in a server.\n\
                  \MESSAGE ID: " <> (showT . messageId $ msg)
              Just uid -> do
                predictionButtonResponse (envDBConnection env) iid itok (read (T.unpack pid) :: Int64) uid msg torf
          _  -> return()
    _ -> return ()
