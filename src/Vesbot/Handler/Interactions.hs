{-# LANGUAGE OverloadedStrings #-}
module Vesbot.Handler.Interactions
  ( interactionHandler
  ) where

import Vesbot.Config (Config(..), Env(..))
import Vesbot.Utils (find, void, liftIO, showT)
import Vesbot.Parsing (getUserData)
import Vesbot.Logging as Logging (log, LogType(..))
import Vesbot.Database (DBConnection)
import Vesbot.BotCommands as BC
import Vesbot.Predictions (predictionButtonResponse)
import Discord
import Discord.Interactions
import Discord.Internal.Types
import qualified Discord.Requests as R

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Int (Int64)

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
    i@InteractionComponent { componentData = b@ButtonData {} } -> buttonInteractionHandler env i
    _ -> return () -- Unexpected/unsupported interaction type

buttonInteractionHandler :: Env -> Interaction -> DiscordHandler ()
buttonInteractionHandler env
  i@InteractionComponent
    { componentData = bdata@ButtonData {}
    , interactionId = iid
    , interactionToken = itok
    , interactionMessage = msg
    , interactionUser = mou
    } =
      case T.splitOn ":" . componentDataCustomId $ bdata of
        ["prediction", pid, torf] -> do
          let muid = getUserData mou
          case muid of
            Nothing ->
              Logging.log Warn $
                "Could not get the user ID of a user that clicked a prediction result button. \
                \This bot is desined to be used in a server."
            Just uid -> do
              predictionButtonResponse (envDBConnection env) iid itok (read (T.unpack pid) :: Int64) uid msg torf
        _  -> return()
